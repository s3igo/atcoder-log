const std = @import("std");

var arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
const stdin = std.io.getStdIn();
const stdout = std.io.getStdOut();
var buffered_stdout = std.io.bufferedWriter(stdout.writer());

pub fn main() !void {
    const allocator = arena.allocator();
    defer arena.deinit();
    const writer = buffered_stdout.writer();
    defer buffered_stdout.flush() catch {};

    const raw_input = try stdin.reader().readAllAlloc(allocator, std.math.maxInt(usize));
    var input = std.mem.tokenizeAny(u8, raw_input, &std.ascii.whitespace);

    const n = try std.fmt.parseInt(u64, input.next().?, 10);
    const as = try allocator.alloc(u64, n);
    for (as) |*a| a.* = try std.fmt.parseInt(u64, input.next().?, 10);

    var cnt: u64 = 0;
    while (cond: {
        for (as) |a| {
            if (a & 1 == 1) break :cond false;
        } else break :cond true;
    }) : (cnt += 1) {
        for (as) |*a| a.* /= 2;
    }

    try writer.print("{d}\n", .{cnt});
}
