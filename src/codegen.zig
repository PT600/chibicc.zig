const std = @import("std");
const Parser = @import("parser.zig");
const Type = @import("type.zig");
const Node = Parser.Node;
const Obj = Parser.Obj;
const FunKind = Parser.FunKind;
const utils = @import("utils.zig");

const GenError = error{ InvalidExpression, InvalidStmt, NotAnLvalue };

const Self = @This();
const arg_regs8 = [_][]const u8{ "%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b" };
const arg_regs32 = [_][]const u8{ "%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d" };
const arg_regs64 = [_][]const u8{ "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };

depth: u32,
count: u8,
cur_func: ?*Obj = null,
output_file: std.fs.File,

pub fn init(output_file: std.fs.File) Self {
    return Self{ .depth = 0, .count = 0, .output_file = output_file };
}

pub fn push(self: *Self) void {
    std.log.debug("push...", .{});
    self.println("  push %rax", .{});
    self.depth += 1;
}

pub fn pop(self: *Self, arg: []const u8) void {
    std.log.debug("pop...", .{});
    self.println("  pop {s}", .{arg});
    self.depth -= 1;
}

pub fn gen(self: *Self, prog: *Obj) anyerror!void {
    try self.emit_data(prog);
    try self.emit_text(prog);
}

fn emit_data(self: *Self, prog: *Obj) anyerror!void {
    var cur: ?*Obj = prog;
    while (cur) |obj| {
        if (obj.as_var()) |v| {
            self.println("  .data", .{});
            self.println("  .globl {s}", .{obj.name});
            self.println("{s}:", .{obj.name});
            if (v.init_data) |init_data| {
                for (init_data) |b| {
                    self.println("  .byte {d}", .{b});
                }
            } else {
                self.println("  .zero {d}", .{obj.ty.size});
            }
        }
        cur = obj.next;
    }
}

fn emit_text(self: *Self, prog: *Obj) anyerror!void {
    var cur: ?*Obj = prog;
    while (cur) |obj| {
        if (obj.as_fun()) |fun| {
            obj.assign_lvar_offsets();
            try self.gen_func(obj, fun);
        }
        cur = obj.next;
    }
}

fn gen_func(self: *Self, obj: *Obj, func: *FunKind) anyerror!void {
    self.cur_func = obj;
    self.println("  .globl {s}", .{obj.name});
    self.println("  .text", .{});
    self.println("{s}:", .{obj.name});
    //Prologue
    self.println("  push %rbp", .{});
    self.println("  mov %rsp, %rbp", .{});
    self.println("  sub ${d}, %rsp", .{func.stack_size});

    var params = func.params;
    var i: usize = 0;
    while (params) |p| {
        std.log.debug("func.params: {s}", .{p.name});
        if (p.as_var()) |v| {
            self.store_gp(i, p.ty.size, v.offset);
        }
        params = p.next;
        i += 1;
    }

    try self.gen_stmt(func.body);
    try utils.assert(self.depth == 0);
    self.println(".L.return.{s}:", .{obj.name});
    self.println("  mov %rbp, %rsp", .{});
    self.println("  pop %rbp", .{});
    self.println("  ret", .{});
}

fn store_gp(self: *Self, i: usize, tysize: usize, offset: i16) void {
    switch (tysize) {
        1 => {
            self.println("  mov {s}, {d}(%rbp)", .{ arg_regs8[i], offset });
        },
        4 => {
            self.println("  mov {s}, {d}(%rbp)", .{ arg_regs32[i], offset });
        },
        8 => {
            self.println("  mov {s}, {d}(%rbp)", .{ arg_regs64[i], offset });
        },
        else => unreachable(),
    }
}

pub fn gen_stmt(self: *Self, node: *Node) anyerror!void {
    std.log.debug("gen_stmt, {}", .{node.kind});
    self.println("  .loc 1 {d}", .{node.tok.?.line});
    switch (node.kind) {
        .Return => {
            const expr_node = node.lhs.?;
            try self.gen_expr(expr_node);
            self.println("  jmp .L.return.{s}", .{self.cur_func.?.name});
        },
        .Stmt => {
            const expr_node = node.lhs.?;
            return self.gen_expr(expr_node);
        },
        .Block => {
            var cur = node.body;
            while (cur) |body| {
                try self.gen_stmt(body);
                cur = body.next;
            }
        },
        .If => {
            const c = self.inc_count();
            const cond = node.cond.?;
            try self.gen_expr(cond);
            self.println("cmp $0, %rax", .{});
            self.println("je .L.else.{d}", .{c});
            try self.gen_stmt(node.then.?);
            self.println("jmp .L.end.{d}", .{c});
            self.println(".L.else.{d}:", .{c});
            if (node.els) |els| {
                try self.gen_stmt(els);
            }
            self.println(".L.end.{d}:", .{c});
        },
        .For => {
            const c = self.inc_count();
            if (node.init) |init_| {
                try self.gen_stmt(init_);
            }
            self.println(".L.begin.{d}:", .{c});
            if (node.cond) |cond| {
                try self.gen_expr(cond);
                self.println("cmp $0, %rax", .{});
                self.println("je .L.end.{d}", .{c});
            }
            try self.gen_stmt(node.then.?);
            if (node.inc) |inc| {
                try self.gen_expr(inc);
            }
            self.println("jmp .L.begin.{d}", .{c});
            self.println(".L.end.{d}:", .{c});
        },
        .While => {
            const c = self.inc_count();
            self.println(".L.begin.{d}:", .{c});
            try self.gen_expr(node.cond.?);
            self.println("cmp $0, %rax", .{});
            self.println("je .L.end.{d}", .{c});
            try self.gen_stmt(node.body.?);
            self.println("jmp .L.begin.{d}", .{c});
            self.println(".L.end.{d}:", .{c});
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
    std.log.debug("gen_expr for node: {?}", .{node});
    self.println("  .loc 1 {d}", .{node.tok.?.line});
    if (try self.gen_unary(node)) return;
    try self.gen_expr(node.rhs.?);
    self.push();

    try self.gen_expr(node.lhs.?);
    self.pop("%rdi");

    switch (node.kind) {
        .Add => {
            self.println("  add %rdi, %rax", .{});
        },
        .Sub => {
            self.println("  sub %rdi, %rax", .{});
        },
        .Mul => {
            self.println("  imul %rdi, %rax", .{});
        },
        .Div => {
            self.println("  cqo", .{});
            self.println("  idiv %rdi", .{});
        },
        .Equal, .NotEqual, .LessThan, .LessEqual => {
            self.println("  cmp %rdi, %rax", .{});
            const opcode = switch (node.kind) {
                .Equal => "sete",
                .NotEqual => "setne",
                .LessThan => "setl",
                .LessEqual => "setle",
                else => unreachable,
            };
            self.println("  {s} %al", .{opcode});
            self.println("  movzb %al, %rax", .{});
        },
        .Comma => {
            try self.gen_expr(node.lhs.?);
            try self.gen_expr(node.rhs.?);
        },
        else => {
            return error.InvalidExpression;
        },
    }
}

fn gen_unary(self: *Self, node: *Node) anyerror!bool {
    switch (node.kind) {
        .Num => {
            self.println("  mov ${d}, %rax", .{node.val});
        },
        .Neg => {
            try self.gen_expr(node.lhs.?);
            self.println(" neg %rax", .{});
        },
        .Addr => {
            try self.gen_addr(node.lhs.?);
        },
        .Deref => {
            try self.gen_expr(node.lhs.?);
            // self.println(" mov (%rax), %rax", .{});
            self.load(node.ty);
        },
        .Var, .Member => {
            try self.gen_addr(node);
            //self.println("  mov (%rax), %rax", .{});
            self.load(node.ty);
        },
        .Assign => {
            try self.gen_addr(node.lhs.?);
            self.push();
            try self.gen_expr(node.rhs.?);
            //self.pop("%rdi");
            //self.println("  mov %rax, (%rdi)", .{});
            self.store(node.ty);
        },
        .StmtExpr => {
            var body = node.body;
            while (body) |b| {
                try self.gen_stmt(b);
                body = b.next;
            }
        },
        .Funcall => {
            var args = node.args;
            var nargs: usize = 0;
            while (args) |arg| {
                try self.gen_expr(arg);
                self.push();
                //self.println("  mov %rax, {s}", .{arg_regs64[nargs]});
                nargs += 1;
                args = arg.next;
            }
            while (nargs > 0) {
                self.pop(arg_regs64[nargs - 1]);
                nargs -= 1;
            }
            self.println("  mov $0, %rax", .{});
            self.println("  call {s}", .{node.funcname.?});
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
            const var_ = node.var_.?;
            if (var_.as_var()) |v| {
                if (v.local) {
                    self.println("  lea {d}(%rbp), %rax", .{v.offset});
                } else {
                    self.println("  lea {s}(%rip), %rax", .{var_.name});
                }
            } else {
                return error.NotAnLvalue;
            }
        },
        .Deref => {
            try self.gen_expr(node.lhs.?);
        },
        .Comma => {
            try self.gen_expr(node.lhs.?);
            try self.gen_addr(node.rhs.?);
        },
        .Member => {
            try self.gen_addr(node.lhs.?);
            self.println("  add ${d}, %rax", .{node.member.?.offset});
        },
        else => return error.NotAnLvalue,
    }
}

fn load(self: *Self, ty: *Type) void {
    switch (ty.kind) {
        .Array, .Struct, .Union => {
            // If it is an array, do not attempt to load a value to the
            // register because in general we can't load an entire array to a
            // register. As a result, the result of an evaluation of an array
            // becomes not the array itself but the address of the array.
            // This is where "array is automatically converted to a pointer to
            // the first element of the array in C" occurs.
            return;
        },
        else => if (ty.size == 1) {
            self.println("  movsbq (%rax), %rax", .{});
        } else if (ty.size == 4) {
            self.println("  movsxd (%rax), %rax", .{});
        } else {
            self.println("  mov (%rax), %rax", .{});
        },
    }
}

// Store %rax to an address that the stack top is pointing to
fn store(self: *Self, ty: *Type) void {
    self.pop("%rdi");
    switch (ty.kind) {
        .Struct, .Union => {
            for (0..ty.size) |i| {
                self.println("  mov {d}(%rax), %r8b", .{i});
                self.println("  mov %r8b, {d}(%rdi)", .{i});
            }
        },
        else => if (ty.size == 1) {
            self.println("  mov %al, (%rdi)", .{});
        } else if (ty.size == 4) {
            self.println("  mov %eax, (%rdi)", .{});
        } else {
            self.println("  mov %rax, (%rdi)", .{});
        },
    }
}

pub fn print(self: *Self, comptime format: []const u8, args: anytype) void {
    self.output_file.writer().print(format, args) catch unreachable;
}

pub fn println(self: *Self, comptime format: []const u8, args: anytype) void {
    self.print(format, args);
    self.print("\n", .{});
}
