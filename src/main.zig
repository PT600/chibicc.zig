const std = @import("std");

const utils = @import("./utils.zig");
const println = utils.println;
const Tokenizer = @import("./tokenizer.zig");
const Parser = @import("./parser.zig");
const Codegen = @import("./codegen.zig");

const Options = struct {
    help: bool,
    err_msg: ?[]const u8,
    input: [*:0]const u8,
    output: [*:0]const u8,

    fn parse(allocator: std.mem.Allocator) !*Options {
        var opts = try allocator.create(Options);
        opts.help = false;
        opts.err_msg = null;
        const argv = std.os.argv;
        var args = argv.ptr;
        const end = args + argv.len;
        var has_input = false;
        while (@ptrToInt(args) < @ptrToInt(end)) : (args += 1) {
            const arg = args[0];
            if (utils.str_eql(arg, "--help")) {
                opts.help = true;
                break;
            } else if (utils.str_eql(arg, "-o")) {
                if (@ptrToInt(args + 1) >= @ptrToInt(end)) {
                    opts.err_msg = try std.fmt.allocPrint(allocator, "invalid output path", .{});
                    break;
                } else {
                    args += 1;
                    opts.output = args[0];
                }
            } else if (utils.str_startswith(arg, "-o")) {
                opts.output = arg + 2;
            } else if (arg[0] == '-' and arg[1] != 0) {
                opts.err_msg = try std.fmt.allocPrint(allocator, "unknown argument: {s}", .{arg});
            } else {
                opts.input = arg;
                has_input = true;
            }
        }
        if (!has_input) {
            opts.err_msg = try std.fmt.allocPrint(allocator, "no input files", .{});
        }
        std.log.debug("opts: input: {s} output: {s}", .{ opts.input, opts.output });
        return opts;
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const opts = try Options.parse(allocator);
    if (opts.help) {
        try std.io.getStdErr().writer().print("chibicc [ -o <path> ] <file>\n", .{});
    } else if (opts.err_msg) |err_msg| {
        try std.io.getStdErr().writer().print("{s}\n", .{err_msg});
    } else {
        const source = try read_file(allocator, opts.input);
        var tokenizer = Tokenizer.init(allocator, source, opts.input);
        const token = try tokenizer.tokenize();
        var parser = Parser.init(allocator, &tokenizer, token);
        const func = try parser.parse();
        const output_file = try std.fs.cwd().createFileZ(opts.output, .{});
        var codegen = Codegen.init(output_file);

        codegen.println(".file 1 \"{s}\"", .{opts.output});
        try codegen.gen(func);
    }
}

fn read_file(allocator: std.mem.Allocator, path: [*:0]const u8) anyerror![*:0]const u8 {
    const fp: std.fs.File = if (path[0] == '-')
        std.io.getStdIn()
    else
        try std.fs.openFileAbsoluteZ(path, .{});
    defer fp.close();
    var buf = std.ArrayList(u8).init(allocator);
    try fp.reader().readAllArrayList(&buf, 1 * 1024 * 1024);
    try buf.append(0);
    return @ptrCast([*:0]const u8, buf.items.ptr);
}
