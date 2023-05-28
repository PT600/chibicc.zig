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

pub const Obj = struct {
    name: []const u8,
    offset: usize = 0,
    next: ?*Obj = null,
    ty: *Type = &Type.TYPE_NONE,
};

pub const Function = struct {
    body: *Node,
    locals: ?*Obj = null,
    stack_size: usize = 0,

    fn assign_lvar_offsets(self: *Function) void {
        var offset: usize = 0;
        var locals = self.locals;
        while (locals) |l| {
            offset += 8;
            l.offset = offset;
            locals = l.next;
        }
        self.stack_size = align_to(offset, 16);
    }

    fn align_to(n: usize, align_: usize) usize {
        return (n + align_ - 1) / align_ * align_;
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

    fn is_ty_pointer(self: *Node) bool {
        return self.ty.kind != .None and self.ty.kind == .Ptr;
    }

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
locals: ?*Obj = null,
scaler: *Node = undefined,

pub fn init(allocator: std.mem.Allocator, tokenizer: *Tokenizer, cur_token: *Token) Self {
    var self = Self{
        .allocator = allocator,
        .tokenizer = tokenizer,
        .cur_token = cur_token,
    };
    self.scaler = self.new_node(.{ .kind = .Num, .val = 8 });
    return self;
}

pub fn parse(self: *Self) anyerror!*Function {
    var head = Node{ .kind = .Stmt };
    var cur = &head;
    while (self.cur_token.kind != .Eof) {
        const node = try self.stmt();
        cur.next = node;
        cur = node;
    }
    const node = head.next.?;
    node.debug(0);
    return self.create_func(node);
}

fn new_add(self: *Self, lhs: *Node, rhs: *Node) !*Node {
    self.add_type(lhs);
    self.add_type(rhs);
    if (lhs.is_ty_integer() and rhs.is_ty_integer()) {
        return self.new_node(.{ .kind = .Add, .lhs = lhs, .rhs = rhs });
    }
    if (lhs.is_ty_integer() and rhs.is_ty_pointer()) {
        const new_rhs = self.new_node(.{ .kind = .Mul, .lhs = self.scaler, .rhs = lhs });
        return self.new_node(.{ .kind = .Add, .lhs = rhs, .rhs = new_rhs });
    }
    if (lhs.is_ty_pointer() and rhs.is_ty_integer()) {
        const new_rhs = self.new_node(.{ .kind = .Mul, .lhs = self.scaler, .rhs = rhs });
        return self.new_node(.{ .kind = .Add, .lhs = lhs, .rhs = new_rhs });
    }
    return error.TokenError;
}

fn new_sub(self: *Self, lhs: *Node, rhs: *Node) !*Node {
    self.add_type(lhs);
    self.add_type(rhs);
    if (lhs.is_ty_integer() and rhs.is_ty_integer()) {
        return self.new_node(.{ .kind = .Sub, .lhs = lhs, .rhs = rhs });
    }
    if (lhs.is_ty_pointer() and rhs.is_ty_integer()) {
        const new_rhs = self.new_node(.{ .kind = .Mul, .lhs = self.scaler, .rhs = rhs });
        self.add_type(new_rhs);
        return self.new_node(.{ .kind = .Sub, .lhs = lhs, .rhs = new_rhs, .ty = lhs.ty });
    }
    if (lhs.is_ty_pointer() and rhs.is_ty_pointer()) {
        const node = self.new_node(.{ .kind = .Sub, .lhs = lhs, .rhs = rhs, .ty = &Type.TYPE_INT });
        return self.new_node(.{ .kind = .Div, .lhs = node, .rhs = self.scaler });
    }
    return error.TokenError;
}

fn create_func(self: *Self, body: *Node) anyerror!*Function {
    const func = try self.allocator.create(Function);
    func.body = body;
    func.locals = self.locals;
    func.assign_lvar_offsets();
    return func;
}

fn new_node(self: *Self, attr: Node) *Node {
    const node = self.allocator.create(Node) catch unreachable;
    node.* = attr;
    if (attr.tok == null)
        node.tok = self.cur_token;
    std.log.debug("new node,ty: {}", .{node.ty});
    return node;
}

fn new_lvar(self: *Self, attr: Obj) *Obj {
    const obj = self.allocator.create(Obj) catch unreachable;
    obj.* = attr;
    obj.next = self.locals;
    self.locals = obj;
    return obj;
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
        self.add_type(cur);
    }
    try self.cur_token_skip("}");
    return self.new_node(.{ .kind = .Block, .body = head.next });
}

// declaration = declspec declarator ("=" expr)? ("," declarator ("=" expr)?)* ";"
fn declaration(self: *Self) anyerror!*Node {
    const basety = try self.declspec();

    var head = Node{ .kind = .Block };
    var cur = &head;
    while (!self.cur_token_match(";")) {
        const ty = try self.declarator(basety);
        var var_ = self.new_lvar(.{ .ty = ty, .name = ty.name.loc });
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

// declarator = "*" * Ident
fn declarator(self: *Self, base: *Type) !*Type {
    _ = self.cur_token_match(",");
    var ty = base;
    while (self.cur_token_match("*")) {
        ty = self.pointer_to(ty);
    }
    if (self.cur_token.kind != .Ident)
        return self.cur_token.error_tok("expect a variable name", .{});
    ty.name = self.cur_token;
    self.advance();
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
    return self.primary();
}

// primary = "(" expr ")" | ident args? | num
fn primary(self: *Self) anyerror!*Node {
    if (self.cur_token.eql("(")) {
        self.advance();
        const node = self.expr();
        self.cur_token = try self.cur_token.skip(")");
        return node;
    }
    if (self.cur_token.kind == .Num) {
        const node = self.new_node(.{ .kind = .Num, .val = self.cur_token.val });
        self.advance();
        return node;
    }
    if (self.cur_token.kind == .Ident) {
        if (token_match(self.cur_token.next, "(")) return self.funcall();
        const node = self.new_node(.{ .kind = .Var });
        node.var_ = self.find_var(self.cur_token) orelse self.new_lvar(.{ .name = self.cur_token.loc });
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
        if (tok.eql(l.name))
            return l;
        locals = l.next;
    }
    return null;
}

fn add_type(self: *Self, node: ?*Node) void {
    if (node) |n| {
        if (n.ty.kind == .None and n.kind != .Eof) {
            self.add_type0(n);
        }
    }
}

fn add_type0(self: *Self, node: *Node) void {
    std.log.debug("add_type0: {}, {}", .{ node.kind, node.ty });
    self.add_type(node.lhs);
    self.add_type(node.rhs);
    self.add_type(node.cond);
    self.add_type(node.then);
    self.add_type(node.els);
    self.add_type(node.init);
    self.add_type(node.inc);

    var body = node.body;
    while (body) |b| {
        if (b.kind == .Eof) break;
        self.add_type(b);
        body = b.next;
    }
    switch (node.kind) {
        .Add, .Sub, .Mul, .Div, .Neg, .Assign => {
            node.ty = node.lhs.?.ty;
        },
        .Addr => {
            node.ty = self.pointer_to(node.lhs.?.ty);
        },
        .Equal, .NotEqual, .LessThan, .LessEqual, .Var, .Num => {
            node.ty = &Type.TYPE_INT;
        },
        .Deref => {
            const lhs = node.lhs.?;
            if (lhs.ty.kind == .Ptr) {
                node.ty = lhs.ty.base.?;
            } else {
                node.ty = &Type.TYPE_INT;
            }
        },
        else => return,
    }
    std.log.debug("add_type0 end: {}, {}", .{ node.kind, node.ty });
}

fn pointer_to(self: *Self, base: *Type) *Type {
    return self.new_type(.{ .kind = .Ptr, .base = base });
}

fn new_type(self: *Self, attr: Type) *Type {
    var ty = self.allocator.create(Type) catch unreachable;
    ty.name = &Token.eof_token;
    ty.* = attr;
    return ty;
}
