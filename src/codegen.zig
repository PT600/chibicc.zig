const std = @import("std");
const Parser = @import("parser.zig");
const Type = @import("type.zig");
const Node = Parser.Node;
const Function = Parser.Function;
const utils = @import("utils.zig");
const println = utils.println;

const GenError = error{ InvalidExpression, InvalidStmt, NotAnLvalue };

const Self = @This();
const arg_regs = [_][]const u8{ "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };

depth: u32,
count: u8,
cur_func: ?*Function = null,

pub fn init() Self {
    return Self{ .depth = 0, .count = 0 };
}

pub fn push(self: *Self) void {
    println("  push %rax", .{});
    self.depth += 1;
}

pub fn pop(self: *Self, arg: []const u8) void {
    println("  pop {s}", .{arg});
    self.depth -= 1;
}

pub fn gen(self: *Self, prog: *Function) anyerror!void {
    var cur: ?*Function = prog;
    while (cur) |func| {
        try self.gen_func(func);
        cur = func.next;
    }
}

fn gen_func(self: *Self, func: *Function) anyerror!void {
    self.cur_func = func;
    println("  .globl {s}", .{func.name});
    println("{s}:", .{func.name});
    //Prologue
    println("  push %rbp", .{});
    println("  mov %rsp, %rbp", .{});
    println("  sub ${d}, %rsp", .{func.stack_size});

    var params = func.params;
    var i: usize = 0;
    while (params) |p| {
        std.log.debug("func.params: {s}", .{p.name});
        println("  mov {s}, {d}(%rbp)", .{ arg_regs[i], p.offset });
        params = p.next;
        i += 1;
    }

    try self.gen_stmt(func.body);
    try utils.assert(self.depth == 0);
    println(".L.return.{s}:", .{func.name});
    println("  mov %rbp, %rsp", .{});
    println("  pop %rbp", .{});
    println("  ret", .{});
}

pub fn gen_stmt(self: *Self, node: *Node) anyerror!void {
    std.log.debug("gen_stmt, {}", .{node.kind});
    switch (node.kind) {
        .Return => {
            const expr_node = node.lhs.?;
            try self.gen_expr(expr_node);
            println("  jmp .L.return.{s}", .{self.cur_func.?.name});
        },
        .Stmt => {
            const expr_node = node.lhs.?;
            return self.gen_expr(expr_node);
        },
        .Block => {
            var cur = node.body;
            while (cur) |body| {
                try self.gen_stmt(body);
                try utils.assert(self.depth == 0);
                cur = body.next;
            }
        },
        .If => {
            const c = self.inc_count();
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
        .For => {
            const c = self.inc_count();
            if (node.init) |init_| {
                try self.gen_stmt(init_);
            }
            println(".L.begin.{d}:", .{c});
            if (node.cond) |cond| {
                try self.gen_expr(cond);
                println("cmp $0, %eax", .{});
                println("je .L.end.{d}", .{c});
            }
            try self.gen_stmt(node.then.?);
            if (node.inc) |inc| {
                try self.gen_expr(inc);
            }
            println("jmp .L.begin.{d}", .{c});
            println(".L.end.{d}:", .{c});
        },
        .While => {
            const c = self.inc_count();
            println(".L.begin.{d}:", .{c});
            try self.gen_expr(node.cond.?);
            println("cmp $0, %eax", .{});
            println("je .L.end.{d}", .{c});
            try self.gen_stmt(node.body.?);
            println("jmp .L.begin.{d}", .{c});
            println(".L.end.{d}:", .{c});
        },
        else => return error.InvalidStmt,
    }
}

fn inc_count(self: *Self) u8 {
    const c = self.count;
    self.count += 1;
    return c;
}

// push right to the stack
// set left to the %rax
// pop the right to the %rdi from stack
// eval the expr to the %rax
pub fn gen_expr(self: *Self, node: *Node) anyerror!void {
    std.log.debug("gen_expr for node: {}", .{node.kind});
    if (try self.gen_unary(node)) return;
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

fn gen_unary(self: *Self, node: *Node) anyerror!bool {
    switch (node.kind) {
        .Num => {
            println("  mov ${d}, %rax", .{node.val});
        },
        .Neg => {
            try self.gen_expr(node.lhs.?);
            println(" neg %rax", .{});
        },
        .Addr => {
            try self.gen_addr(node.lhs.?);
        },
        .Deref => {
            try self.gen_expr(node.lhs.?);
            // println(" mov (%rax), %rax", .{});
            self.load(node.ty);
        },
        .Var => {
            try self.gen_addr(node);
            //println("  mov (%rax), %rax", .{});
            self.load(node.ty);
        },
        .Assign => {
            try self.gen_addr(node.lhs.?);
            self.push();
            try self.gen_expr(node.rhs.?);
            self.pop("%rdi");
            println("  mov %rax, (%rdi)", .{});
        },
        .Funcall => {
            var args = node.args;
            var nargs: usize = 0;
            while (args) |arg| {
                try self.gen_expr(arg);
                self.push();
                //println("  mov %rax, {s}", .{arg_regs[nargs]});
                nargs += 1;
                args = arg.next;
            }
            while (nargs > 0) {
                self.pop(arg_regs[nargs - 1]);
                nargs -= 1;
            }
            println("  mov $0, %rax", .{});
            println("  call {s}", .{node.funcname.?});
        },
        else => {
            return false;
        },
    }
    return true;
}

fn gen_addr(self: *Self, node: *Node) anyerror!void {
    //_ = self;
    switch (node.kind) {
        .Var => {
            const offset = node.var_.?.offset;
            println("  lea {d}(%rbp), %rax", .{offset});
        },
        .Deref => {
            try self.gen_expr(node.lhs.?);
        },
        else => return error.NotAnLvalue,
    }
}

fn load(self: *Self, ty: *Type) void {
    _ = self;
    if (ty.kind == .Array) {
        // If it is an array, do not attempt to load a value to the
        // register because in general we can't load an entire array to a
        // register. As a result, the result of an evaluation of an array
        // becomes not the array itself but the address of the array.
        // This is where "array is automatically converted to a pointer to
        // the first element of the array in C" occurs.
        return;
    }
    println("  mov (%rax), %rax", .{});
}

// Store %rax to an address that the stack top is pointing to
fn store(self: *Self) void {
    self.pop("%rdi");

    println("  mov %rax, (%rdi)", .{});
}
