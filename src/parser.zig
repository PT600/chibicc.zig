const std = @import("std");
const utils = @import("utils.zig");
const Tokenizer = @import("tokenizer.zig");
const Token = Tokenizer.Token;
const TokenKind = Tokenizer.TokenKind;
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
    StmtExpr,
    If,
    For,
    While,
    Eof,
    Assign,
    Comma, //,
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

pub const VarKind = struct {
    offset: i16 = 0,
    local: bool = true,
    init_data: ?[]const u8 = null,
};

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
        std.log.debug("*" ** 40, .{});
        std.log.debug("obj.name: {s}, ty: {?}", .{ self.name, self.ty });
        if (self.as_fun()) |fun| {
            std.log.debug("function, params: {?}", .{fun.params});
            fun.body.debug(0);
        }
    }
};

const TABS = " " ** 256;
pub const Node = struct {
    kind: NodeKind,
    tok: ?*Token = null,
    next: ?*Node = null,
    lhs: ?*Node = null,
    rhs: ?*Node = null,
    // block or Statement Expression
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
        return self.ty.kind == .Int or self.ty.kind == .Char;
    }

    // fn is_ty_pointer(self: *Node) bool {
    //     return self.ty.kind != .None and self.ty.kind == .Ptr;
    // }

    fn debug(self: *Node, depth: u8) void {
        const tab = TABS[0..(depth * 3)];
        std.log.debug("{s} {}, tok: {d} ty: {}", .{ tab, self.kind, self.tok.?, self.ty.kind });
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
        if (self.cond) |cond| {
            std.log.debug("{s} cond:", .{tab});
            cond.debug(depth + 1);
        }
        if (self.then) |then| {
            std.log.debug("{s} then:", .{tab});
            then.debug(depth + 1);
        }
        if (self.els) |els| {
            std.log.debug("{s} els:", .{tab});
            els.debug(depth + 1);
        }
        if (self.kind != .Eof and self.next != null) {
            std.log.debug("{s} next:", .{tab});
            self.next.?.debug(depth + 1);
        }
    }
};

const Scope = struct {
    vars: std.ArrayList(*Obj),
    next: ?*Scope = null,

    fn find_var(self: Scope, tok: *Token) ?*Obj {
        std.log.debug("scope.vars.len: {d}", .{self.vars.items.len});
        for (self.vars.items) |v| {
            if (std.mem.eql(u8, v.name, tok.loc))
                return v;
        }
        return null;
    }
};

const Self = @This();

allocator: std.mem.Allocator,
tokenizer: *Tokenizer,
cur_token: *Token,
locals: ?*Obj = null,
globals: ?*Obj = null,
name_idx: u8 = 0,
scope: *Scope,

pub fn init(allocator: std.mem.Allocator, tokenizer: *Tokenizer, cur_token: *Token) *Self {
    const self = allocator.create(Self) catch unreachable;
    self.allocator = allocator;
    self.tokenizer = tokenizer;
    self.cur_token = cur_token;
    self.locals = null;
    self.globals = null;
    self.name_idx = 0;
    self.scope = self.alloc(Scope, .{ .vars = std.ArrayList(*Obj).init(allocator) });
    return self;
}

// program = (function-definition | global-variable)*
pub fn parse(self: *Self) anyerror!*Obj {
    while (self.cur_token.kind != .Eof) {
        var basety = self.declspec().?;
        var decl = try self.declarator(basety);
        if (decl.is_function) {
            _ = try self.function(decl.ty, decl.name);
        } else {
            _ = self.global_variable(decl.ty, decl.name, .{});
            while (!self.cur_token_match(";")) {
                try self.cur_token_skip(",");
                decl = try self.declarator(basety);
                _ = self.global_variable(decl.ty, decl.name, .{});
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

fn new_var(self: *Self, ty: *Type, name: []const u8, kind: VarKind) *Obj {
    const obj = self.alloc(Obj, .{ .ty = ty, .name = name, .kind = .{ .Var = kind } });
    self.push_scope(obj);
    return obj;
}

fn global_variable(self: *Self, ty: *Type, name: []const u8, kind: VarKind) *Obj {
    var obj = self.new_var(ty, name, kind);
    obj.kind.Var.local = false;
    obj.next = self.globals;
    self.globals = obj;
    return obj;
}

// function = "(" (declspec declarator ",")* ")" "{" body "}";
fn function(self: *Self, ty: *Type, name: []const u8) anyerror!*Obj {
    std.log.debug("parse function: {s}, ty: {?}", .{ name, ty });
    const fun = self.new_func();
    fun.ty = ty;
    fun.name = name;
    self.enter_scope();
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
    fun.debug();
    self.leave_scope();
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
    if (self.cur_token_match2("if")) |tok| {
        try self.cur_token_skip("(");
        const cond = try self.expr();
        try self.cur_token_skip(")");
        const then = try self.stmt();
        const els = if (self.cur_token_match("else"))
            try self.stmt()
        else
            null;
        return self.new_node(.{ .kind = .If, .cond = cond, .then = then, .els = els, .tok = tok });
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
    self.enter_scope();
    while (!self.cur_token.eql("}")) {
        const node = if (self.declspec()) |basety|
            try self.declaration(basety)
        else
            try self.stmt();
        cur.next = node;
        cur = node;
        //self.add_type(cur, 0);
    }
    try self.cur_token_skip("}");
    self.leave_scope();
    return self.new_node(.{ .kind = .Block, .body = head.next });
}

// declaration = declarator ("["num"]")? ("=" expr)? ("," declarator ("=" expr)?)* ";"
fn declaration(self: *Self, basety: *Type) anyerror!*Node {
    var head = Node{ .kind = .Block };
    var cur = &head;
    while (!self.cur_token_match(";")) {
        var decl = try self.declarator(basety);
        var var_ = self.new_var(decl.ty, decl.name, .{});
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

fn declspec(self: *Self) ?*Type {
    if (self.cur_token_match("int")) {
        return &Type.TYPE_INT;
    } else if (self.cur_token_match("char")) {
        return &Type.TYPE_CHAR;
    }
    return null;
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
        var basety = self.declspec().?;
        var decl = try self.declarator(basety);
        const var_ = self.new_var(decl.ty, decl.name, .{});
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

// expr = assign ("," expr)?
pub fn expr(self: *Self) anyerror!*Node {
    var node = try self.assign();
    if (self.cur_token_match2(",")) |tok| {
        const rhs = try self.expr();
        return self.new_node(.{ .kind = .Comma, .lhs = node, .rhs = rhs, .tok = tok });
    }
    return node;
}

// assign = identity (= assign)*
fn assign(self: *Self) anyerror!*Node {
    var node = try self.equality();
    if (self.cur_token_match2("=")) |tok| {
        const right = try self.assign();
        node = self.new_node(.{ .kind = .Assign, .lhs = node, .rhs = right, .tok = tok });
    }
    return node;
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
    if (self.cur_token_match2("*")) |tok| {
        const node = try self.unary();
        return self.new_node(.{ .kind = .Deref, .lhs = node, .tok = tok });
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

// primary = "(" "{"stmt"}" | expr ")" | ident args? | num
fn primary(self: *Self) anyerror!*Node {
    if (self.cur_token_match("(")) {
        if (self.cur_token_match("{")) {
            const block = try self.block_stmt();
            const node = self.new_node(.{ .kind = .StmtExpr, .body = block.body });
            try self.cur_token_skip(")");
            return node;
        }
        const node = try self.expr();
        try self.cur_token_skip(")");
        return node;
    }
    if (self.cur_token_match("sizeof")) {
        const node = try self.unary();
        return self.new_num(node.ty.size);
    }
    if (self.cur_token_match_kind(.Num)) |tok| {
        const node = self.new_num(tok.val);
        return node;
    }
    if (self.cur_token_match_kind(.Ident)) |tok| {
        if (self.cur_token_match("(")) return self.funcall(tok.loc);
        const var_ = self.find_var(tok) orelse return error.ParseError;
        const node = self.new_node(.{ .kind = .Var, .var_ = var_, .tok = tok });
        return node;
    }
    if (self.cur_token_match_kind(.Str)) |tok| {
        const name = try self.new_unique_name();

        const init_data0 = tok.loc[1..(tok.loc.len - 1)];
        var init_data = self.parse_str(init_data0);
        const ty = self.new_array_type(&Type.TYPE_CHAR, init_data.len);
        const var_ = self.global_variable(ty, name, .{ .init_data = init_data });
        const node = self.new_node(.{ .kind = .Var, .var_ = var_ });
        return node;
    }
    return self.cur_token.error_tok("expected an expression\n", .{});
}

fn parse_str(self: *Self, str: []const u8) []const u8 {
    var buf = self.allocator.alloc(u8, str.len + 1) catch unreachable;
    var len: usize = 0;
    var p = str.ptr;
    var end = p + str.len;
    while (@ptrToInt(p) < @ptrToInt(end)) {
        if (p[0] == '\\') {
            p += 1;
            if ('0' <= p[0] and p[0] <= '7') {
                buf[len] = read_octal_number(&p);
            } else if ('x' == p[0]) {
                buf[len] = read_hex_number(&p);
            } else {
                buf[len] = read_escaped_char(p[0]);
                p += 1;
            }
            len += 1;
        } else {
            buf[len] = p[0];
            p += 1;
            len += 1;
        }
    }
    buf[len] = 0;
    len += 1;
    return buf[0..len];
}

fn read_hex_number(pp: *[*]const u8) u8 {
    var p = pp.*;
    p += 1;
    var c: u8 = 0;
    while (from_hex(p[0])) |hex| {
        c = (c << 4) + hex;
        p += 1;
    }
    pp.* = p;
    return c;
}

fn from_hex(c: u8) ?u8 {
    if ('0' <= c and c <= '9')
        return c - '0';
    if ('a' <= c and c <= 'f')
        return c - 'a' + 10;
    if ('A' <= c and c <= 'F')
        return c - 'A' + 10;
    return null;
}

fn read_octal_number(pp: *[*]const u8) u8 {
    var p = pp.*;
    var c = p[0] - '0';
    p += 1;
    if ('0' <= p[0] and p[0] <= '7') {
        c = (c << 3) + p[0] - '0';
        p += 1;
        if ('0' <= p[0] and p[0] <= '7') {
            c = (c << 3) + p[0] - '0';
            p += 1;
        }
    }
    pp.* = p;
    return c;
}

fn read_escaped_char(c: u8) u8 {
    return switch (c) {
        'a' => 0x07,
        'b' => 0x08,
        't' => 0x09,
        'n' => 0x0A,
        'v' => 0x0B,
        'f' => 0x0C,
        'r' => 0x0D,
        'e' => 27,
        else => c,
    };
}

fn new_unique_name(self: *Self) anyerror![]const u8 {
    self.name_idx += 1;
    return std.fmt.allocPrint(self.allocator, ".L..{d}", .{self.name_idx});
}

fn funcall(self: *Self, funcname: []const u8) anyerror!*Node {
    const node = self.new_node(.{ .kind = .Funcall, .funcname = funcname });
    var head = Node{ .kind = .Stmt };
    var cur = &head;
    while (!self.cur_token_match(")")) {
        const arg = try self.assign();
        cur.next = arg;
        cur = arg;
        _ = self.cur_token_match(",");
    }
    node.args = head.next;
    return node;
}

fn cur_token_match2(self: *Self, tok: []const u8) ?*Token {
    if (self.cur_token.eql(tok)) {
        defer self.advance();
        return self.cur_token;
    }
    return null;
}

fn cur_token_match(self: *Self, tok: []const u8) bool {
    if (self.cur_token.eql(tok)) {
        self.advance();
        return true;
    }
    return false;
}

fn cur_token_match_kind(self: *Self, kind: TokenKind) ?*Token {
    if (self.cur_token.kind == kind) {
        const token = self.cur_token;
        self.advance();
        return token;
    }
    return null;
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
    var scope: ?*Scope = self.scope;
    while (scope) |s| {
        if (s.find_var(tok)) |var_|
            return var_;
        scope = s.next;
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
            node.debug(0);
            node.ty = node.lhs.?.ty.base.?;
        },
        .Stmt => {
            node.ty = node.lhs.?.ty;
        },
        .StmtExpr => {
            var body = node.body.?;
            while (body.next) |b| {
                body = b;
            }
            node.ty = body.ty;
        },
        .Comma => {
            node.ty = node.rhs.?.ty;
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

fn push_scope(self: *Self, var_: *Obj) void {
    self.scope.vars.append(var_) catch unreachable;
}

fn enter_scope(self: *Self) void {
    var scope = self.alloc(Scope, .{ .vars = std.ArrayList(*Obj).init(self.allocator) });
    scope.next = self.scope;
    self.scope = scope;
}

fn leave_scope(self: *Self) void {
    self.scope = self.scope.next.?;
}

pub fn alloc(self: *Self, comptime T: type, defaults: T) *T {
    return utils.alloc(self.allocator, T, defaults);
}
