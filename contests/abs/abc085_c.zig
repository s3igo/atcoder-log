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
    const y = try std.fmt.parseInt(u64, input.next().?, 10);

    for (0..n + 1) |i| {
        for (0..n + 1 - i) |j| {
            const k = n - i - j;
            if (10000 * i + 5000 * j + 1000 * k == y) {
                try writer.print("{d} {d} {d}\n", .{ i, j, k });
                return;
            }
        }
    }

    try writer.print("-1 -1 -1\n", .{});
}
