const std = @import("std");
const utils = @import("utils.zig");
const Errors = utils.Errors;
const errwriter = utils.errwriter;

pub const TokenKind = enum {
    Punct,
    Num,
    Eof,
};

pub const Token = struct {
    kind: TokenKind,
    loc: []const u8,
    next: *Token = undefined,
    val: i32 = 0,

    pub fn eql(self: *Token, s: []const u8) bool {
        return std.mem.eql(u8, self.loc, s);
    }

    pub fn skip(self: *Token, s: []const u8) !*Token {
        if (!self.eql(s)) {
            return self.error_tok("expected '{s}'\n", .{s});
        }
        return self.next;
    }

    pub fn get_number(self: *Token) !i32 {
        if (self.kind != .Num) {
            try self.error_tok("expected a number!\n", .{});
        }
        return self.val;
    }

    pub fn error_tok(tok: *Token, comptime format: []const u8, args: anytype) anyerror {
        try errwriter.print("error token '{s}' ", .{tok.loc});
        try errwriter.print(format, args);
        return error.TokenError;
    }
};

fn isspace(c: u8) bool {
    return c == ' ' or c == '\r' or c == '\n';
}

fn isdigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn ispunct(c: u8) bool {
    return c == '+' or c == '-' or c == '*' or c == '/' or c == '(' or c == ')';
}

const Self = @This();

allocator: std.mem.Allocator,
source: [*:0]const u8,
eof_token: *Token,

pub fn init(allocator: std.mem.Allocator, source: [*:0]const u8) Self {
    const token = allocator.create(Token) catch unreachable;
    token.kind = .Eof;
    token.loc = "";
    return Self{
        .allocator = allocator,
        .source = source,
        .eof_token = token,
    };
}

pub fn error_at(self: *Self, loc: [*]const u8, comptime format: []const u8, args: anytype) anyerror {
    var pos = @ptrToInt(loc) - @ptrToInt(self.source);
    try errwriter.print("{s}\n", .{self.source});
    while (pos > 0) : (pos -= 1) {
        try errwriter.print(" ", .{});
    }
    try errwriter.print("^ ", .{});
    try errwriter.print(format, args);
    return error.TokenError;
}

fn new_token(self: *Self, attr: Token) *Token {
    const token = self.allocator.create(Token) catch unreachable;
    token.* = attr;
    token.next = self.eof_token;
    return token;
}

pub fn tokenize(self: *Self) anyerror!*Token {
    var p = self.source;
    var head = Token{ .kind = .Eof, .loc = "" };
    var cur = &head;
    while (p[0] != 0) {
        if (isspace(p[0])) {
            p += 1;
            continue;
        }

        if (isdigit(p[0])) {
            const start = p;
            const val = try utils.strtol(p, &p);
            const len = @ptrToInt(p) - @ptrToInt(start);
            const loc = start[0..len];
            const token = self.new_token(.{ .kind = .Num, .loc = loc, .val = val });
            cur.next = token;
            cur = token;
            continue;
        }
        if (ispunct(p[0])) {
            const loc = p[0..1];
            const token = self.new_token(.{ .kind = .Punct, .loc = loc });
            cur.next = token;
            cur = token;
            p += 1;
            continue;
        }

        return self.error_at(p, "invalid token", .{});
    }

    return head.next;
}
