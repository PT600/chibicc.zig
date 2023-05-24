const std = @import("std");
const Self = @This();

pub const TypeKind = enum(u8) {
    Int,
    Ptr,
    None,
};

pub const TYPE_INT = &Self{ .kind = .Int };
pub const TYPE_NONE = &Self{ .kind = .None };

kind: TypeKind,
base: ?*const Self = null,

pub fn is_integer(self: *Self) bool {
    return self.kind == .Int;
}
