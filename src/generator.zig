const std = @import("std");
const Parser = @import("parser.zig");
const Node = Parser.Node;
const utils = @import("utils.zig");
const println = utils.println;

const GenError = error{InvalidExpression};

const Self = @This();

depth: u32,

pub fn init() Self {
    return Self{ .depth = 0 };
}

pub fn push(self: *Self) void {
    println("  push %rax", .{});
    self.depth += 1;
}

pub fn pop(self: *Self, arg: []const u8) void {
    println("  pop {s}", .{arg});
    self.depth -= 1;
}

pub fn gen_expr(self: *Self, node: *Node) !void {
    if (node.kind == .Num) {
        println("  mov ${d}, %rax", .{node.val});
        return;
    }
    const right = node.rhs orelse return error.TokenError;
    try self.gen_expr(right);
    self.push();

    const left = node.lhs orelse return error.TokenError;
    try self.gen_expr(left);
    self.pop("%rdi");

    switch (node.kind) {
        .Add => {
            println("  add %rdi, %rax", .{});
        },
        .Sub => {
            println("  sub %rdi, %rax", .{});
        },
        .Mul => {
            println("  imul %rdi, %rax", .{});
        },
        .Div => {
            println("  cqo", .{});
            println("  idiv %rdi", .{});
        },
        else => {
            return error.InvalidExpression;
        },
    }
}
