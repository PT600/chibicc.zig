const std = @import("std");

const Token = @import("tokenizer.zig").Token;

pub const TypeKind = enum(u8) {
    Char,
    Int,
    Long,
    Short,
    Ptr,
    Func,
    Array,
    Struct,
    Union,
    Void,
    None,
};

const Self = @This();

pub var TYPE_VOID = Self{ .kind = .Void, .size = 1, .align_ = 1 };
pub var TYPE_CHAR = Self{ .kind = .Char, .size = 1, .align_ = 1 };
pub var TYPE_INT = Self{ .kind = .Int, .size = 4, .align_ = 4 };
pub var TYPE_LONG = Self{ .kind = .Long, .size = 8, .align_ = 8 };
pub var TYPE_SHORT = Self{ .kind = .Short, .size = 2, .align_ = 2 };
pub var TYPE_NONE = Self{ .kind = .None, .size = 0, .align_ = 0 };

pub const Member = struct {
    ty: *Self,
    name: *Token,
    offset: usize = 0,
};

kind: TypeKind,
base: ?*Self = null,
return_ty: ?*Self = null,
params: ?*Self = null,
align_: usize,
// struct
members: ?[]*Member = null,
tag: ?*Token = null,
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
    return switch (self.kind) {
        .Int, .Char, .Long, .Short => true,
        else => false,
    };
}

pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
    return writer.print("{?}, {?}", .{ self.kind, self.base });
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
pub fn align_to(n: usize, align_: usize) usize {
    return (n + align_ - 1) / align_ * align_;
}
