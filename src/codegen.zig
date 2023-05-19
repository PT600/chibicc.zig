const std = @import("std");
const Parser = @import("parser.zig");
const Node = Parser.Node;
const Function = Parser.Function;
const utils = @import("utils.zig");
const println = utils.println;

const GenError = error{ InvalidExpression, InvalidStmt, NotAnLvalue };

const Self = @This();

depth: u32,
if_count: u8,

pub fn init() Self {
    return Self{ .depth = 0, .if_count = 0 };
}

pub fn push(self: *Self) void {
    println("  push %rax", .{});
    self.depth += 1;
}

pub fn pop(self: *Self, arg: []const u8) void {
    println("  pop {s}", .{arg});
    self.depth -= 1;
}

pub fn gen(self: *Self, prog: *Function) !void {
    println("  .globl main", .{});
    println("main:", .{});

    //Prologue
    println("  push %rbp", .{});
    println("  mov %rsp, %rbp", .{});
    println("  sub ${d}, %rsp", .{prog.stack_size});

    try self.gen_stmt(prog.body);
    try utils.assert(self.depth == 0);
    println(".L.return:", .{});
    println("  mov %rbp, %rsp", .{});
    println("  pop %rbp", .{});
    println("  ret", .{});
}

pub fn gen_stmt(self: *Self, node: *Node) !void {
    switch (node.kind) {
        .Return => {
            const expr_node = node.lhs.?;
            try self.gen_expr(expr_node);
            println("  jmp .L.return", .{});
        },
        .Stmt => {
            const expr_node = node.lhs.?;
            return self.gen_expr(expr_node);
        },
        .Block => {
            if (node.body) |body| {
                var cur = body;
                while (cur.kind != .Eof) : (cur = cur.next) {
                    try self.gen_stmt(cur);
                    try utils.assert(self.depth == 0);
                }
            }
        },
        .If => {
            const c = self.if_count;
            self.if_count += 1;
            const cond = node.cond.?;
            try self.gen_expr(cond);
            println("cmp $0, %eax", .{});
            println("je .L.else.{d}", .{c});
            try self.gen_stmt(node.then.?);
            println("je .L.end.{d}", .{c});
            println(".L.else.{d}:", .{c});
            if (node.els) |els| {
                try self.gen_stmt(els);
            }
            println(".L.end.{d}:", .{c});
        },
        else => return error.InvalidStmt,
    }
}

// push right to the stack
// set left to the %rax
// pop the right to the %rdi from stack
// eval the expr to the %rax
pub fn gen_expr(self: *Self, node: *Node) !void {
    std.log.debug("gen_expr for node: {}\n", .{node.kind});
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
        .Var => {
            try self.gen_addr(node);
            println("  mov (%rax), %rax", .{});
            return;
        },
        .Assign => {
            try self.gen_addr(node.lhs.?);
            self.push();

            try self.gen_expr(node.rhs.?);
            self.pop("%rdi");
            println("  mov %rax, (%rdi)", .{});
            return;
        },
        else => {},
    }
    try self.gen_expr(node.rhs.?);
    self.push();

    try self.gen_expr(node.lhs.?);
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

fn gen_addr(self: *Self, node: *Node) !void {
    _ = self;
    switch (node.kind) {
        .Var => {
            const offset = node.var_.?.offset;
            println("  lea -{d}(%rbp), %rax", .{offset});
        },
        else => return error.NotAnLvalue,
    }
}
