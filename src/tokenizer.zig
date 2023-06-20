const std = @import("std");
const utils = @import("utils.zig");
const Errors = utils.Errors;
const errwriter = utils.errwriter;

pub const TokenKind = enum {
    Punct,
    Ident,
    Keyword,
    Num,
    Str,
    Eof,
};

pub const Token = struct {
    pub var eof_token = Token{ .kind = .Eof, .loc = "" };
    kind: TokenKind,
    loc: []const u8,
    line: usize = 0,
    next: *Token = undefined,
    val: i32 = 0,

    pub fn eql(self: *Token, s: []const u8) bool {
        return std.mem.eql(u8, self.loc, s);
    }

    pub fn skip(self: *Token, s: []const u8) anyerror!*Token {
        if (!self.eql(s)) {
            return self.error_tok("expected '{s}'\n", .{s});
        }
        return self.next;
    }

    pub fn get_number(self: *Token) anyerror!i32 {
        if (self.kind != .Num) {
            return self.error_tok("expected a number!\n", .{});
        }
        return self.val;
    }

    pub fn error_tok(tok: *Token, comptime fmt: []const u8, args: anytype) anyerror {
        try errwriter.print("error token '{s}' ", .{tok.loc});
        try errwriter.print(fmt ++ "\n", args);
        return error.TokenError;
    }

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return writer.print("loc: {s}, line: {d}", .{ self.loc, self.line });
    }
};

fn isspace(c: u8) bool {
    return c == ' ' or c == '\r' or c == '\n';
}

fn isdigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn ispunct(c: u8) bool {
    return switch (c) {
        '+',
        '-',
        '*',
        '/',
        '(',
        ')',
        '=',
        '<',
        '>',
        '!',
        ';',
        '{',
        '}',
        '&',
        ',',
        '[',
        ']',
        '.',
        => true,
        else => false,
    };
}

fn isalpha(c: u8) bool {
    return 'a' <= c and c <= 'z' or c == '_';
}

const KEYWORDS = [_][]const u8{
    "return",
    "if",
    "else",
    "for",
    "while",
    "sizeof",
    "char",
    "struct",
};

fn is_keyword(word: []const u8) bool {
    for (KEYWORDS) |keyword| {
        if (std.mem.eql(u8, keyword, word))
            return true;
    }
    return false;
}

const Self = @This();

allocator: std.mem.Allocator,
source: [*:0]const u8,
cur_filename: [*:0]const u8,
cur_line: usize = 0,

pub fn init(allocator: std.mem.Allocator, source: [*:0]const u8, path: [*:0]const u8) Self {
    std.log.info("file: {s}, source: {s}", .{ path, source });
    return Self{
        .allocator = allocator,
        .source = source,
        .cur_filename = path,
        .cur_line = 0,
    };
}

pub fn debug_token(self: *Self, token: *Token) !void {
    _ = self;
    var cur = token;
    while (cur.kind != .Eof) {
        //std.log.debug("{}: {s}", .{ cur.kind, cur.loc });
        cur = cur.next;
    }
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
    token.next = &Token.eof_token;
    token.line = self.cur_line;
    return token;
}

pub fn tokenize(self: *Self) anyerror!*Token {
    var p = self.source;
    var head = Token{ .kind = .Eof, .loc = "" };
    var cur = &head;
    while (p[0] != 0) {
        if (p[0] == '\n' or p[0] == '\r') {
            self.cur_line += 1;
        }
        if (isspace(p[0])) {
            p += 1;
        } else if (isdigit(p[0])) {
            const start = p;
            const val = try utils.strtol(p, &p);
            const len = @ptrToInt(p) - @ptrToInt(start);
            const loc = start[0..len];
            cur.next = self.new_token(.{ .kind = .Num, .loc = loc, .val = val });
            cur = cur.next;
        } else if (utils.str_startswith(p, "//")) {
            p += 2;
            while (p[0] != '\n') {
                p += 1;
            }
        } else if (utils.str_startswith(p, "/*")) {
            const start = p;
            p += 2;
            while (p[0] != 0) : (p += 1) {
                if (p[0] == '*' and p[1] == '/') {
                    p += 2;
                    break;
                }
            } else {
                return self.error_at(start, "unclosed block comment", .{});
            }
        } else if (ispunct(p[0])) {
            const punc_len: u8 = if (p[1] == '=' and (p[0] == '=' or p[0] == '!' or p[0] == '>' or p[0] == '<')) 2 else 1;
            const loc = p[0..punc_len];
            cur.next = self.new_token(.{ .kind = .Punct, .loc = loc });
            cur = cur.next;
            p += punc_len;
        } else if (isalpha(p[0])) {
            var pp = p;
            while (isalpha(pp[0]) or isdigit(pp[0])) {
                pp += 1;
            }
            const end = @ptrToInt(pp) - @ptrToInt(p);
            const loc = p[0..end];
            cur.next = self.new_token(.{ .kind = .Ident, .loc = loc });
            cur = cur.next;
            convert_keyword(cur);
            p = pp;
        } else if (p[0] == '"') {
            var pp = p + 1;
            while (pp[0] != '"') : (pp += 1) {
                if (pp[0] == '\n') // or pp[0] == '\0')
                    return self.error_at(pp, "unclosed string literal", .{});
                if (pp[0] == '\\')
                    pp += 1;
            }
            const end = @ptrToInt(pp + 1) - @ptrToInt(p);
            const loc = p[0..end];
            cur.next = self.new_token(.{ .kind = .Str, .loc = loc });
            cur = cur.next;
            p = pp + 1;
        } else {
            return self.error_at(p, "invalid token: {}", .{p[0] == '\\'});
        }
    }
    try self.debug_token(head.next);
    return head.next;
}

pub fn convert_keyword(self: *Token) void {
    if (is_keyword(self.loc)) {
        self.kind = .Keyword;
    }
}
