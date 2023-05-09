const std = @import("std");
const writer = std.io.getStdOut().writer();
const utils = @import("./utils.zig");
const Allocator = std.mem.Allocator;

fn print(comptime format: []const u8, args: anytype) void {
    writer.print(format, args) catch unreachable;
}

fn println(comptime format: []const u8, args: anytype) void {
    print(format, args);
    print("\n", .{});
}

fn error0(comptime format: []const u8, args: anytype) anyerror {
    try std.io.getStdErr().writer().print(format, args);
    return error.ParseError;
}

const ParseError = error{ InvalidArgument, UnexpectedCharacter, ParseError };

const TokenKind = enum {
    Punct,
    Num,
    Eof,
};

const Token = struct {
    kind: TokenKind,
    loc: []const u8,
    next: ?*Token = null,
    val: i32 = 0,

    fn eql(self: *Token, s: []const u8) bool {
        return std.mem.eql(u8, self.loc, s);
    }

    fn skip(self: *Token, s: []const u8) !?*Token {
        if (!self.eql(s)) {
            return error0("expected '{s}'\n", .{s});
        }
        return self.next;
    }

    fn get_number(self: *Token) !i32 {
        if (self.kind != .Num) {
            return error0("expected a number\n", .{});
        }
        return self.val;
    }

    fn new(allocator: Allocator, attr: Token) *Token {
        const token = allocator.create(Token) catch unreachable;
        token.* = attr;
        return token;
    }
};

fn isspace(c: u8) bool {
    return c == ' ' or c == '\r' or c == '\n';
}

fn isdigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn tokenize(allocator: Allocator, source: [*]const u8) !*Token {
    var p = source;
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
            const token = Token.new(allocator, .{ .kind = .Num, .loc = loc, .val = val });
            cur.next = token;
            cur = token;
            continue;
        }
        if (p[0] == '+' or p[0] == '-') {
            const loc = p[0..1];
            const token = Token.new(allocator, .{ .kind = .Punct, .loc = loc });
            cur.next = token;
            cur = token;
            p += 1;
            continue;
        }

        return error0("invalid token", .{});
    }
    const token = Token.new(allocator, .{ .kind = .Eof, .loc = "" });
    cur.next = token;
    cur = token;

    return head.next.?;
}

pub fn main() !void {
    const argv = std.os.argv;
    if (argv.len != 2) {
        return error.InvalidArgument;
    }
    var p = argv[1];

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var first_token = try tokenize(allocator, p);

    println("  .globl main", .{});
    println("main:", .{});
    // the first token must be a number
    var num = try first_token.get_number();
    println("  mov ${d}, %rax", .{num});
    var next = first_token.next;

    while (next) |token| {
        if (token.kind == .Eof) break;
        if (token.eql("+")) {
            const t = token.next orelse return error0("expected token", .{});
            num = try t.get_number();
            println("  add ${d}, %rax", .{num});
            next = t.next;
            continue;
        }
        const t = try token.skip("-") orelse return error0("expected token", .{});
        num = try t.get_number();
        println("  sub ${d}, %rax", .{num});
        next = t.next;
    }

    println("  ret", .{});
}
