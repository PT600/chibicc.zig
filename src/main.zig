const std = @import("std");
const writer = std.io.getStdOut().writer();
const utils = @import("./utils.zig");

fn print(comptime format: []const u8, args: anytype) void {
    writer.print(format, args) catch unreachable;
}

fn println(comptime format: []const u8, args: anytype) void {
    print(format, args);
    print("\n", .{});
}

const Error = error{ InvalidArgument, UnexpectedCharacter };

pub fn main() !void {
    const argv = std.os.argv;
    if (argv.len != 2) {
        return error.InvalidArgument;
    }
    var p = argv[1];
    var num = try utils.strtol(p, &p);
    println("  .globl main", .{});
    println("main:", .{});
    println("  mov ${d}, %rax", .{num});
    while (p[0] != 0) {
        if (p[0] == '+') {
            p += 1;
            num = try utils.strtol(p, &p);
            println("  add ${d}, %rax", .{num});
            continue;
        }
        if (p[0] == '-') {
            p += 1;
            num = try utils.strtol(p, &p);
            println("  sub ${d}, %rax", .{num});
            continue;
        }

        return error.UnexpectedCharacter;
    }
    println("  ret", .{});
}
