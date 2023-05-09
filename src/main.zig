const std = @import("std");
const writer = std.io.getStdOut().writer();

fn print(comptime format: []const u8, args: anytype) void {
    writer.print(format, args) catch unreachable;
}

fn println(comptime format: []const u8, args: anytype) void {
    print(format, args);
    print("\n", .{});
}

const Error = error{InvalidArgument};

pub fn main() !void {
    const argv = std.os.argv;
    if (argv.len != 2) {
        return error.InvalidArgument;
    }
    println("  .globl main", .{});
    println("main:", .{});
    println("  mov ${s}, %rax", .{argv[1]});
    println("  ret", .{});
}
