const std = @import("std");
const Parser = @import("parser.zig");
const Node = Parser.Node;
const utils = @import("utils.zig");
const println = utils.println;

const GenError = error{ InvalidExpression, InvalidStmt };

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

pub fn gen(self: *Self, node: *Node) !void {
    println("  .globl main", .{});
    println("main:", .{});

    var cur = node;
    while (cur.kind != .Eof) : (cur = cur.next) {
        try self.gen_stmt(cur);
        try utils.assert(self.depth == 0);
    }
    println("  ret", .{});
}

pub fn gen_stmt(self: *Self, node: *Node) !void {
    if (node.kind == .Stmt) {
        const expr_node = node.lhs.?;
        return self.gen_expr(expr_node);
    }
    return error.InvalidStmt;
}

// push right to the stack
// set left to the %rax
// pop the right to the %rdi from stack
// eval the expr to the %rax
pub fn gen_expr(self: *Self, node: *Node) !void {
    switch (node.kind) {
        .Num => {
            println("  mov ${d}, %rax", .{node.val});
            return;
        },
        .Neg => {
            try self.gen_expr(node.lhs.?);
            println(" neg %rax", .{});
            return;
        },
        else => {},
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
        .Equal, .NotEqual, .LessThan, .LessEqual => {
            println("  cmp %rdi, %rax", .{});
            const opcode = switch (node.kind) {
                .Equal => "sete",
                .NotEqual => "setne",
                .LessThan => "setl",
                .LessEqual => "setle",
                else => unreachable,
            };
            println("  {s} %al", .{opcode});
            println("  movzb %al, %rax", .{});
        },
        else => {
            return error.InvalidExpression;
        },
    }
}
