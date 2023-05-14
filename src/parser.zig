const std = @import("std");
const utils = @import("utils.zig");
const Tokenizer = @import("tokenizer.zig");
const Token = Tokenizer.Token;

pub const NodeKind = enum {
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
    Eof,
    Assign,
    Var,
};

pub const Obj = struct {
    name: []const u8,
    offset: usize = 0,
    next: ?*Obj = null,
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

pub const Node = struct {
    kind: NodeKind,
    next: *Node = undefined,
    lhs: ?*Node = null,
    rhs: ?*Node = null,
    var_: ?*Obj = null,
    val: i32 = 0,
};

const Self = @This();

allocator: std.mem.Allocator,
tokenizer: *Tokenizer,
cur_token: *Token,
eof_node: *Node,
locals: ?*Obj = null,

pub fn init(allocator: std.mem.Allocator, tokenizer: *Tokenizer, cur_token: *Token) Self {
    var self = Self{
        .allocator = allocator,
        .tokenizer = tokenizer,
        .cur_token = cur_token,
        .eof_node = undefined,
    };
    self.eof_node = self.new_node(.{ .kind = .Eof });
    return self;
}

pub fn parse(self: *Self) anyerror!*Function {
    var head = Node{ .kind = .Stmt };
    var cur = &head;
    while (self.cur_token.kind != .Eof) {
        cur.next = try self.stmt();
        cur = cur.next;
    }
    return self.create_func(head.next);
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
    node.next = self.eof_node;
    return node;
}

fn new_obj(self: *Self, attr: Obj) *Obj {
    const obj = self.allocator.create(Obj) catch unreachable;
    obj.* = attr;
    obj.next = self.locals;
    self.locals = obj;
    return obj;
}

pub fn stmt(self: *Self) anyerror!*Node {
    var node = try self.expr();
    _ = try self.cur_token.skip(";");
    self.advance();
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
            node = self.new_node(.{ .kind = .Add, .lhs = node, .rhs = right_node });
            continue;
        }
        if (self.cur_token_match("-")) {
            const right_node = try self.mul();
            node = self.new_node(.{ .kind = .Sub, .lhs = node, .rhs = right_node });
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

fn unary(self: *Self) anyerror!*Node {
    if (self.cur_token.eql("-")) {
        self.advance();
        const node = try self.unary();
        return self.new_node(.{ .kind = .Neg, .lhs = node });
    }
    if (self.cur_token.eql("+")) {
        self.advance();
        return self.unary();
    }
    return self.primary();
}

// primary = "(" expr ")" | ident | num
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
        const obj = self.find_var(self.cur_token) orelse self.new_obj(.{ .name = self.cur_token.loc });
        const node = self.new_node(.{ .kind = .Var, .var_ = obj });
        self.advance();
        return node;
    }
    return self.cur_token.error_tok("expected an expression\n", .{});
}

fn cur_token_match(self: *Self, tok: []const u8) bool {
    if (self.cur_token.eql(tok)) {
        self.advance();
        return true;
    }
    return false;
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
