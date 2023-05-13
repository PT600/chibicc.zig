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
    Stmt,
    Eof,
    Assign,
    Var,
    // Note: no > and >=
};

pub const Node = struct {
    kind: NodeKind,
    next: *Node = undefined,
    lhs: ?*Node = null,
    rhs: ?*Node = null,
    name: ?[]const u8 = null,
    val: i32 = 0,
};

const Self = @This();

allocator: std.mem.Allocator,
tokenizer: *Tokenizer,
cur_token: *Token,
eof_node: *Node,

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

pub fn parse(self: *Self) anyerror!*Node {
    var head = Node{ .kind = .Stmt };
    var cur = &head;
    while (self.cur_token.kind != .Eof) {
        cur.next = try self.stmt();
        cur = cur.next;
    }
    return head.next;
}

fn new_node(self: *Self, attr: Node) *Node {
    const node = self.allocator.create(Node) catch unreachable;
    node.* = attr;
    node.next = self.eof_node;
    return node;
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
        const node = self.new_node(.{ .kind = .Var, .name = self.cur_token.loc });
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
