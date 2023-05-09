const std = @import("std");

pub fn strtol(source: [*]const u8, new_p: *[*]const u8) !i32 {
    var p = source;
    while (p[0] >= '0' and p[0] <= '9') {
        p += 1;
    }
    const len = @ptrToInt(p) - @ptrToInt(source);
    new_p.* = p;
    return try std.fmt.parseInt(i32, source[0..len], 10);
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
