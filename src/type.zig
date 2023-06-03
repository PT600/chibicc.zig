const std = @import("std");

const Token = @import("tokenizer.zig").Token;

pub const TypeKind = enum(u8) {
    Int,
    Ptr,
    Func,
    Array,
    None,
};

const Self = @This();

pub var TYPE_INT = Self{ .kind = .Int, .size = 8 };
pub var TYPE_NONE = Self{ .kind = .None, .size = 0 };

kind: TypeKind,
base: ?*Self = null,
return_ty: ?*Self = null,
params: ?*Self = null,
// Array
array_len: usize = 0,
// Pointer
size: usize, // sizeof() value
// Pointer-to or array-of type. We intentionally use the same member
// to represent pointer/array duality in C.
//
// In many contexts in which a pointer is expected, we examine this
// member instead of "kind" member to determine whether a type is a
// pointer or not. That means in many contexts "array of T" is
// naturally handled as if it were "pointer to T", as required by
// the C spec.

pub fn is_integer(self: *Self) bool {
    return self.kind == .Int;
}

pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
    return writer.print("{?}, {?}", .{ self.kind, self.base });
}
