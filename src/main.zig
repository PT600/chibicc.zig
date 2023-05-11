const std = @import("std");

const utils = @import("./utils.zig");
const println = utils.println;
const Tokenizer = @import("./tokenizer.zig");
const Parser = @import("./parser.zig");
const Codegen = @import("./codegen.zig");

pub fn main() !void {
    const argv = std.os.argv;
    if (argv.len != 2) {
        return error.InvalidArgument;
    }

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var tokenizer = Tokenizer.init(allocator, argv[1]);
    const token = try tokenizer.tokenize();
    var parser = Parser.init(allocator, &tokenizer, token);
    const node = try parser.expr();
    var codegen = Codegen.init();

    println("  .globl main", .{});
    println("main:", .{});

    try codegen.gen_expr(node);
    println("  ret", .{});

    try utils.assert(codegen.depth == 0);
}
