const std = @import("std");
const utils = @import("utils.zig");
const Tokenizer = @import("tokenizer.zig");
const Token = Tokenizer.Token;
const Type = @import("type.zig");

pub const NodeKind = enum(u8) {
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Num,
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    // Note: no > and >=
    Stmt,
    If,
    For,
    While,
    Eof,
    Assign,
    Return,
    Block,
    Var,
    Addr, // &
    Deref, // *
    Funcall,
};

pub const FunKind = struct {
    body: *Node,
    params: ?*Obj = null,
    locals: ?*Obj = null,
    stack_size: usize = 0,
};

pub const VarKind = struct { offset: i16 = 0, local: bool = true };

pub const ObjKind = union(enum) { Var: VarKind, Fun: FunKind };

pub const Obj = struct {
    name: []const u8,
    kind: ObjKind,
    ty: *Type = &Type.TYPE_NONE,
    next: ?*Obj = null,

    pub fn as_fun(self: *Obj) ?*FunKind {
        if (self.kind == .Fun) {
            return &self.kind.Fun;
        }
        return null;
    }

    pub fn as_var(self: *Obj) ?*VarKind {
        if (self.kind == .Var) {
            return &self.kind.Var;
        }
        return null;
    }

    pub fn assign_lvar_offsets(self: *Obj) void {
        if (self.as_fun()) |fun| {
            var offset: usize = 0;
            var locals = fun.locals;
            while (locals) |l| {
                offset += l.ty.size;
                if (l.as_var()) |v| {
                    v.offset = -@intCast(i16, offset);
                }
                locals = l.next;
            }
            fun.stack_size = align_to(offset, 16);
        }
    }

    fn align_to(n: usize, align_: usize) usize {
        return (n + align_ - 1) / align_ * align_;
    }

    fn debug(self: *Obj) void {
        var cur: ?*Obj = self;
        while (cur) |obj| {
            std.log.debug("*" ** 40, .{});
            std.log.debug("obj.name: {s}", .{obj.name});
            if (obj.as_fun()) |fun| {
                std.log.debug("obj.params: {?}", .{fun.params});
                fun.body.debug(0);
            }
            cur = obj.next;
        }
    }
};

const TABS = "                                    ";
pub const Node = struct {
    kind: NodeKind,
    next: ?*Node = null,
    tok: ?*Token = null,
    lhs: ?*Node = null,
    rhs: ?*Node = null,
    // block
    body: ?*Node = null,

    // "if" statment
    cond: ?*Node = null,
    then: ?*Node = null,
    els: ?*Node = null,

    init: ?*Node = null,
    inc: ?*Node = null,
    var_: ?*Obj = null,
    val: i32 = 0,
    ty: *Type = &Type.TYPE_NONE,
    funcname: ?[]const u8 = null,
    args: ?*Node = null,

    fn is_ty_integer(self: *Node) bool {
        return self.ty.kind != .None and self.ty.kind == .Int;
    }

    // fn is_ty_pointer(self: *Node) bool {
    //     return self.ty.kind != .None and self.ty.kind == .Ptr;
    // }

    fn debug(self: *Node, depth: u8) void {
        const tab = TABS[0..(depth * 3)];
        std.log.debug("{s} {}, ty: {}", .{ tab, self.kind, self.ty.kind });
        if (self.lhs) |lhs| {
            std.log.debug("{s} lhs:", .{tab});
            lhs.debug(depth + 1);
        }
        if (self.rhs) |rhs| {
            std.log.debug("{s} rhs:", .{tab});
            rhs.debug(depth + 1);
        }
        if (self.body) |body| {
            std.log.debug("{s} body:", .{tab});
            body.debug(depth + 1);
        }
        if (self.kind != .Eof and self.next != null) {
            std.log.debug("{s} next:", .{tab});
            self.next.?.debug(depth + 1);
        }
    }
};

const Self = @This();

allocator: std.mem.Allocator,
tokenizer: *Tokenizer,
cur_token: *Token,
locals: ?*Obj,
globals: ?*Obj,

pub fn init(allocator: std.mem.Allocator, tokenizer: *Tokenizer, cur_token: *Token) *Self {
    const self = allocator.create(Self) catch unreachable;
    self.allocator = allocator;
    self.tokenizer = tokenizer;
    self.cur_token = cur_token;
    self.locals = null;
    self.globals = null;
    return self;
}

// program = (function-definition | global-variable)*
pub fn parse(self: *Self) anyerror!*Obj {
    while (self.cur_token.kind != .Eof) {
        var basety = try self.declspec();
        var decl = try self.declarator(basety);
        if (decl.is_function) {
            _ = try self.function(decl.ty, decl.name);
        } else {
            _ = try self.global_variable(decl.ty, decl.name);
            while (!self.cur_token_match(";")) {
                try self.cur_token_skip(",");
                decl = try self.declarator(basety);
                _ = try self.global_variable(decl.ty, decl.name);
            }
        }
    }
    if (self.globals) |g| {
        g.debug();
        return g;
    }
    return error.ParseError;
}

fn new_add(self: *Self, lhs: *Node, rhs: *Node) !*Node {
    //self.add_type(lhs, 0);
    //self.add_type(rhs, 0);
    if (lhs.is_ty_integer() and rhs.is_ty_integer()) {
        return self.new_node(.{ .kind = .Add, .lhs = lhs, .rhs = rhs });
    }
    // num + ptr
    if (lhs.is_ty_integer()) {
        if (rhs.ty.base) |base| {
            const num = self.new_num(base.size);
            const new_rhs = self.new_node(.{ .kind = .Mul, .lhs = num, .rhs = lhs });
            return self.new_node(.{ .kind = .Add, .lhs = rhs, .rhs = new_rhs });
        }
    }
    // ptr + num
    if (lhs.ty.base) |base| {
        if (rhs.is_ty_integer()) {
            const num = self.new_num(base.size);
            const new_rhs = self.new_node(.{ .kind = .Mul, .lhs = num, .rhs = rhs });
            return self.new_node(.{ .kind = .Add, .lhs = lhs, .rhs = new_rhs });
        }
    }
    std.log.debug("lhs: {} {}, rhs: {} {}", .{ lhs.kind, lhs.ty, rhs.kind, rhs.ty });
    return error.TokenError;
}

fn new_sub(self: *Self, lhs: *Node, rhs: *Node) !*Node {
    //self.add_type(lhs, 0);
    //self.add_type(rhs, 0);
    if (lhs.is_ty_integer() and rhs.is_ty_integer()) {
        return self.new_node(.{ .kind = .Sub, .lhs = lhs, .rhs = rhs });
    }
    // ptr - num
    if (lhs.ty.base != null and rhs.is_ty_integer()) {
        const num = self.new_num(lhs.ty.base.?.size);
        const new_rhs = self.new_node(.{ .kind = .Mul, .lhs = num, .rhs = rhs });
        //self.add_type(new_rhs, 0);
        return self.new_node(.{ .kind = .Sub, .lhs = lhs, .rhs = new_rhs });
    }

    // ptr - ptr
    if (lhs.ty.base != null and rhs.ty.base != null) {
        const num = self.new_num(rhs.ty.base.?.size);
        const new_lhs = self.new_node(.{ .kind = .Sub, .lhs = lhs, .rhs = rhs, .ty = &Type.TYPE_INT });
        return self.new_binary(.Div, new_lhs, num);
    }
    return error.TokenError;
}

fn new_func(self: *Self) *Obj {
    const fun = self.allocator.create(Obj) catch unreachable;
    return fun;
}

fn new_num(self: *Self, val: anytype) *Node {
    return self.new_node(.{ .kind = .Num, .val = @intCast(i32, val), .ty = &Type.TYPE_INT });
}

fn new_binary(self: *Self, kind: NodeKind, lhs: *Node, rhs: *Node) *Node {
    return self.new_node(.{ .kind = kind, .lhs = lhs, .rhs = rhs });
}

fn new_node(self: *Self, attr: Node) *Node {
    const node = self.allocator.create(Node) catch unreachable;
    node.* = attr;
    if (attr.tok == null)
        node.tok = self.cur_token;
    self.parse_type(node);
    return node;
}

fn new_var(self: *Self, ty: *Type, name: []const u8, local: bool) *Obj {
    const obj = self.allocator.create(Obj) catch unreachable;
    obj.ty = ty;
    obj.name = name;
    obj.next = null;
    obj.kind = ObjKind{ .Var = .{ .offset = 0, .local = local } };
    return obj;
}

fn global_variable(self: *Self, ty: *Type, name: []const u8) anyerror!*Obj {
    var obj = self.new_var(ty, name, false);
    obj.next = self.globals;
    self.globals = obj;
    return obj;
}

// function = declspec declarator "(" (declspec declarator ",")* ")";
fn function(self: *Self, ty: *Type, name: []const u8) anyerror!*Obj {
    const fun = self.new_func();
    fun.ty = ty;
    fun.name = name;
    const params = try self.func_params();
    self.locals = params;
    try self.cur_token_skip("{");
    const body = try self.block_stmt();
    fun.kind = ObjKind{ .Fun = FunKind{
        .params = params,
        .body = body,
        .locals = self.locals,
    } };
    fun.next = self.globals;
    self.globals = fun;
    return fun;
}

// stmt = "return" expr ";"
//      | "if" (expr) stmt (else stmt)?
//      | "for"(init?;cond?;inc?) stmt
//      | "while (cond) stmt
//      |  "{" compound_stmt "}"
//      |  expr_stmt
pub fn stmt(self: *Self) anyerror!*Node {
    if (self.cur_token_match("return")) {
        var node = try self.expr();
        try self.cur_token_skip(";");
        return self.new_node(.{ .kind = .Return, .lhs = node });
    }
    if (self.cur_token_match("if")) {
        try self.cur_token_skip("(");
        const cond = try self.expr();
        try self.cur_token_skip(")");
        const then = try self.stmt();
        const els = if (self.cur_token_match("else"))
            try self.stmt()
        else
            null;
        return self.new_node(.{ .kind = .If, .cond = cond, .then = then, .els = els });
    }
    if (self.cur_token_match("for")) {
        try self.cur_token_skip("(");
        var node = self.new_node(.{ .kind = .For });
        node.init = try self.expr_stmt();
        if (!self.cur_token_match(";")) {
            node.cond = try self.expr();
            try self.cur_token_skip(";");
        }
        if (!self.cur_token_match(")")) {
            node.inc = try self.expr();
            try self.cur_token_skip(")");
        }
        node.then = try self.stmt();
        return node;
    }
    if (self.cur_token_match("while")) {
        try self.cur_token_skip("(");
        const cond = try self.expr();
        try self.cur_token_skip(")");
        const body = try self.stmt();
        return self.new_node(.{ .kind = .While, .cond = cond, .body = body });
    }
    if (self.cur_token_match("{")) {
        return self.block_stmt();
    }
    return self.expr_stmt();
}

// block_stmt = (declaration | stmt)* "}"
fn block_stmt(self: *Self) anyerror!*Node {
    var head = Node{ .kind = .Stmt };
    var cur = &head;
    while (!self.cur_token.eql("}")) {
        const node = if (self.cur_token.eql("int"))
            try self.declaration()
        else
            try self.stmt();
        cur.next = node;
        cur = node;
        //self.add_type(cur, 0);
    }
    try self.cur_token_skip("}");
    return self.new_node(.{ .kind = .Block, .body = head.next });
}

// declaration = declspec declarator ("["num"]")? ("=" expr)? ("," declarator ("=" expr)?)* ";"
fn declaration(self: *Self) anyerror!*Node {
    const basety = try self.declspec();

    var head = Node{ .kind = .Block };
    var cur = &head;
    while (!self.cur_token_match(";")) {
        var decl = try self.declarator(basety);
        var var_ = self.new_var(decl.ty, decl.name, true);
        var_.next = self.locals;
        self.locals = var_;
        if (self.cur_token_match("=")) {
            const rhs = try self.assign();
            const lhs = self.new_node(.{ .kind = .Var, .var_ = var_ });
            const assign_node = self.new_node(.{ .kind = .Assign, .lhs = lhs, .rhs = rhs });
            const node = self.new_node(.{ .kind = .Stmt, .lhs = assign_node });
            cur.next = node;
            cur = node;
        }
    }
    const node = self.new_node(.{ .kind = .Block, .body = head.next });
    return node;
}

fn declspec(self: *Self) !*Type {
    try self.cur_token_skip("int");
    return &Type.TYPE_INT;
}

const Declarator = struct {
    ty: *Type,
    name: []const u8,
    is_function: bool,
};

// declarator = "*" * Ident
fn declarator(self: *Self, basety: *Type) !Declarator {
    _ = self.cur_token_match(",");
    var ty = basety;
    while (self.cur_token_match("*")) {
        ty = self.pointer_to(ty);
    }
    const name = try self.consume_ident();
    var is_function = true;
    if (!self.cur_token.eql("(")) {
        is_function = false;
        ty = try self.array_suffix(ty);
    }
    return .{ .ty = ty, .name = name, .is_function = is_function };
}

fn consume_ident(self: *Self) ![]const u8 {
    if (self.cur_token.kind != .Ident)
        return self.cur_token.error_tok("expect a variable name", .{});
    const tok = self.cur_token;
    self.advance();
    return tok.loc;
}

// fucn-params = ("(" params ")")?
fn func_params(self: *Self) !?*Obj {
    var head = Obj{ .name = "head", .kind = undefined };
    var cur = &head;
    try self.cur_token_skip("(");
    while (!self.cur_token_match(")")) {
        var basety = try self.declspec();
        var decl = try self.declarator(basety);
        const var_ = self.new_var(decl.ty, decl.name, true);
        cur.next = var_;
        cur = var_;
        _ = self.cur_token_match(",");
    }
    return head.next;
}

fn array_suffix(self: *Self, basety: *Type) !*Type {
    var ty = basety;
    if (self.cur_token_match("[")) {
        const len = @intCast(usize, try self.cur_token.get_number());
        self.advance();
        try self.cur_token_skip("]");
        ty = try self.array_suffix(ty);
        ty = self.new_array_type(ty, len);
    }
    return ty;
}

// expr-stmt = expr? ";"
fn expr_stmt(self: *Self) anyerror!*Node {
    if (self.cur_token.eql(";")) {
        self.advance();
        return self.new_node(.{ .kind = .Block });
    }
    var node = try self.expr();
    try self.cur_token_skip(";");
    return self.new_node(.{ .kind = .Stmt, .lhs = node });
}

pub fn expr(self: *Self) anyerror!*Node {
    return self.assign();
}

// assign = identity (= assign)*
fn assign(self: *Self) anyerror!*Node {
    var node = try self.equality();
    while (true) {
        if (self.cur_token_match("=")) {
            const right = try self.assign();
            node = self.new_node(.{ .kind = .Assign, .lhs = node, .rhs = right });
            continue;
        }
        return node;
    }
}

// equality = relational ( "==|!=" relational) *
pub fn equality(self: *Self) anyerror!*Node {
    var node = try self.relational();
    while (true) {
        if (self.cur_token_match("==")) {
            const right = try self.relational();
            node = self.new_node(.{ .kind = .Equal, .lhs = node, .rhs = right });
            continue;
        }
        if (self.cur_token_match("!=")) {
            const right = try self.relational();
            node = self.new_node(.{ .kind = .NotEqual, .lhs = node, .rhs = right });
            continue;
        }
        return node;
    }
}
// relational = add ( "<|<=|>|>=" add) *
pub fn relational(self: *Self) anyerror!*Node {
    var node = try self.add();
    while (true) {
        if (self.cur_token_match("<")) {
            const right = try self.add();
            node = self.new_node(.{ .kind = .LessThan, .lhs = node, .rhs = right });
            continue;
        }
        if (self.cur_token_match("<=")) {
            const right = try self.add();
            node = self.new_node(.{ .kind = .LessEqual, .lhs = node, .rhs = right });
            continue;
        }
        if (self.cur_token_match(">")) {
            const right = try self.add();
            node = self.new_node(.{ .kind = .LessThan, .lhs = right, .rhs = node });
            continue;
        }
        if (self.cur_token_match(">=")) {
            const right = try self.add();
            node = self.new_node(.{ .kind = .LessEqual, .lhs = right, .rhs = node });
            continue;
        }
        return node;
    }
}

// add = mul ("+"|"-" mul) *
pub fn add(self: *Self) anyerror!*Node {
    var node = try self.mul();
    while (true) {
        if (self.cur_token_match("+")) {
            const right_node = try self.mul();
            //node = self.new_node(.{ .kind = .Add, .lhs = node, .rhs = right_node });
            node = try self.new_add(node, right_node);
            continue;
        }
        if (self.cur_token_match("-")) {
            const right_node = try self.mul();
            //node = self.new_node(.{ .kind = .Sub, .lhs = node, .rhs = right_node });
            node = try self.new_sub(node, right_node);
            continue;
        }
        return node;
    }
}
// mul = primary ("*|/" primary) *
fn mul(self: *Self) anyerror!*Node {
    var node = try self.unary();
    while (true) {
        if (self.cur_token.eql("*")) {
            self.advance();
            const right_node = try self.unary();
            node = self.new_node(.{ .kind = .Mul, .lhs = node, .rhs = right_node });
            continue;
        }
        if (self.cur_token.eql("/")) {
            self.advance();
            const right_node = try self.unary();
            node = self.new_node(.{ .kind = .Div, .lhs = node, .rhs = right_node });
            continue;
        }
        return node;
    }
}

// unary = ("+" | "-" | "*" | "&" ) unary
fn unary(self: *Self) anyerror!*Node {
    if (self.cur_token_match("-")) {
        const node = try self.unary();
        return self.new_node(.{ .kind = .Neg, .lhs = node });
    }
    if (self.cur_token_match("+")) {
        return self.unary();
    }
    if (self.cur_token_match("&")) {
        const node = try self.unary();
        return self.new_node(.{ .kind = .Addr, .lhs = node });
    }
    if (self.cur_token_match("*")) {
        const node = try self.unary();
        return self.new_node(.{ .kind = .Deref, .lhs = node });
    }
    return self.postfix();
}

fn postfix(self: *Self) anyerror!*Node {
    var node = try self.primary();
    while (self.cur_token_match("[")) {
        const idx = try self.expr();
        node = try self.new_add(node, idx);
        node = self.new_node(.{ .kind = .Deref, .lhs = node });
        try self.cur_token_skip("]");
    }
    return node;
}

// primary = "(" expr ")" | ident args? | num
fn primary(self: *Self) anyerror!*Node {
    if (self.cur_token_match("(")) {
        const node = try self.expr();
        self.cur_token = try self.cur_token.skip(")");
        return node;
    }
    if (self.cur_token_match("sizeof")) {
        const node = try self.unary();
        return self.new_num(node.ty.size);
    }
    if (self.cur_token.kind == .Num) {
        const node = self.new_num(self.cur_token.val);
        self.advance();
        return node;
    }
    if (self.cur_token.kind == .Ident) {
        if (token_match(self.cur_token.next, "(")) return self.funcall();
        const var_ = self.find_var(self.cur_token) orelse return error.ParseError;
        const node = self.new_node(.{ .kind = .Var, .var_ = var_ });
        self.advance();
        return node;
    }
    return self.cur_token.error_tok("expected an expression\n", .{});
}

fn funcall(self: *Self) anyerror!*Node {
    const node = self.new_node(.{ .kind = .Funcall, .funcname = self.cur_token.loc });
    self.advance();
    self.advance();
    var head = Node{ .kind = .Stmt };
    var cur = &head;
    while (!self.cur_token_match(")")) {
        const arg = try self.expr();
        cur.next = arg;
        cur = arg;
        _ = self.cur_token_match(",");
    }
    node.args = head.next;
    return node;
}

fn cur_token_match(self: *Self, tok: []const u8) bool {
    if (self.cur_token.eql(tok)) {
        self.advance();
        return true;
    }
    return false;
}

fn token_match(tok: ?*Token, name: []const u8) bool {
    if (tok) |t| {
        return std.mem.eql(u8, t.loc, name);
    }
    return false;
}

fn cur_token_skip(self: *Self, tok: []const u8) !void {
    self.cur_token = try self.cur_token.skip(tok);
}

fn advance(self: *Self) void {
    self.cur_token = self.cur_token.next;
}

fn peek(self: *Self) *Node {
    return self.cur_token.next;
}

fn find_var(self: *Self, tok: *Token) ?*Obj {
    var locals = self.locals;
    while (locals) |l| {
        if (tok.eql(l.name)) return l;
        locals = l.next;
    }
    var globals = self.globals;
    while (globals) |l| {
        if (tok.eql(l.name)) return l;
        globals = l.next;
    }
    return null;
}

fn parse_type(self: *Self, node: *Node) void {
    if (node.ty.kind != .None) return;
    switch (node.kind) {
        .Add, .Sub, .Mul, .Div, .Neg, .Assign => {
            node.ty = node.lhs.?.ty;
        },
        .Addr => {
            const lhs = node.lhs.?;
            if (lhs.ty.kind == .Array) {
                node.ty = self.pointer_to(lhs.ty.base.?);
            } else {
                node.ty = self.pointer_to(lhs.ty);
            }
        },
        .Equal, .NotEqual, .LessThan, .LessEqual, .Num, .Funcall => {
            node.ty = &Type.TYPE_INT;
        },
        .Var => {
            node.ty = node.var_.?.ty;
        },
        .Deref => {
            node.ty = node.lhs.?.ty.base.?;
        },
        else => return,
    }
}

fn pointer_to(self: *Self, base: *Type) *Type {
    return self.new_type(.{ .kind = .Ptr, .base = base, .size = 8 });
}

fn new_type(self: *Self, attr: Type) *Type {
    var ty = self.allocator.create(Type) catch unreachable;
    ty.* = attr;
    return ty;
}

fn new_array_type(self: *Self, basety: *Type, len: usize) *Type {
    const size = basety.size * len;
    return self.new_type(.{ .kind = .Array, .base = basety, .array_len = len, .size = size });
}
