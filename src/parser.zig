const std = @import("std");
const utils = @import("utils.zig");
const Tokenizer = @import("tokenizer.zig");
const Token = Tokenizer.Token;

pub const NodeKind = enum {
    Add,
    Sub,
    Mul,
    Div,
    Num,
};

pub const Node = struct {
    kind: NodeKind,
    lhs: ?*Node = null,
    rhs: ?*Node = null,
    val: i32 = 0,
};

const Self = @This();

allocator: std.mem.Allocator,
tokenizer: *Tokenizer,
cur_token: *Token,

pub fn init(allocator: std.mem.Allocator, tokenizer: *Tokenizer, cur_token: *Token) Self {
    return Self{
        .allocator = allocator,
        .tokenizer = tokenizer,
        .cur_token = cur_token,
    };
}

fn new_node(self: *Self, attr: Node) *Node {
    const node = self.allocator.create(Node) catch unreachable;
    node.* = attr;
    return node;
}

// expr = mul ("+"|"-" mul) *
pub fn expr(self: *Self) anyerror!*Node {
    var node = try self.mul();
    while (true) {
        if (self.cur_token.eql("+")) {
            self.advance();
            const right_node = try self.mul();
            node = self.new_node(.{ .kind = .Add, .lhs = node, .rhs = right_node });
            continue;
        }
        if (self.cur_token.eql("-")) {
            self.advance();
            const right_node = try self.mul();
            node = self.new_node(.{ .kind = .Sub, .lhs = node, .rhs = right_node });
            continue;
        }
        return node;
    }
}
// mul = primary ("*|/" primary) *
fn mul(self: *Self) anyerror!*Node {
    var node = try self.primary();
    while (true) {
        if (self.cur_token.eql("*")) {
            self.advance();
            const right_node = try self.primary();
            node = self.new_node(.{ .kind = .Mul, .lhs = node, .rhs = right_node });
            continue;
        }
        if (self.cur_token.eql("/")) {
            self.advance();
            const right_node = try self.primary();
            node = self.new_node(.{ .kind = .Div, .lhs = node, .rhs = right_node });
            continue;
        }
        return node;
    }
}

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
    return self.cur_token.error_tok("expected an expression", .{});
}

fn advance(self: *Self) void {
    self.cur_token = self.cur_token.next;
}
