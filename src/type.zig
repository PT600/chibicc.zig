const std = @import("std");

const Token = @import("tokenizer.zig").Token;

pub const TypeKind = enum(u8) {
    Int,
    Ptr,
    None,
};
const Self = @This();

pub var TYPE_INT = Self{ .kind = .Int };
pub var TYPE_NONE = Self{ .kind = .None };

kind: TypeKind,
base: ?*Self = null,
name: *const Token = &Token.eof_token,

pub fn is_integer(self: *Self) bool {
    return self.kind == .Int;
}

pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
    return writer.print("{?}, {?}, {?}", .{ self.kind, self.name.kind, self.base });
}
