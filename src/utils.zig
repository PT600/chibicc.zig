const std = @import("std");
pub const writer = std.io.getStdOut().writer();
pub const errwriter = std.io.getStdErr().writer();

pub fn assert(v: bool) !void {
    if (!v) {
        return error.AssertError;
    }
}

pub const Errors = error{ InvalidArgument, UnexpectedCharacter, ParseError, AssertError, TokenError };

pub fn strtol(source: [*]const u8, new_p: *[*]const u8) !i32 {
    var p = source;
    while (p[0] >= '0' and p[0] <= '9') {
        p += 1;
    }
    const len = @ptrToInt(p) - @ptrToInt(source);
    new_p.* = p;
    return try std.fmt.parseInt(i32, source[0..len], 10);
}

pub fn str_startswith(src: [*:0]const u8, dest: []const u8) bool {
    var p = src;
    for (dest) |c| {
        if (p[0] == 0 or p[0] != c) return false;
        p += 1;
    }
    return true;
}
pub fn str_eql(src: [*:0]const u8, dest: []const u8) bool {
    var p = src;
    for (dest) |c| {
        if (p[0] == 0 or p[0] != c) return false;
        p += 1;
    }
    return p[0] == 0;
}

pub fn alloc(allocator: std.mem.Allocator, comptime T: type, defaults: T) *T {
    var t = allocator.create(T) catch unreachable;
    t.* = defaults;
    return t;
}

test "strtol" {
    var p: [*]const u8 = "123 + 345";
    var l = try strtol(p, &p);
    try std.testing.expect(l == 123);
    try std.testing.expect(p[0] == ' ');

    p = "12*34";
    l = try strtol(p, &p);
    try std.testing.expect(l == 12);
    try std.testing.expect(p[0] == '*');
}
