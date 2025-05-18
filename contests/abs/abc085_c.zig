const std = @import("std");

pub fn main() !void {
    var arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const stdin = std.io.getStdIn();
    var buffered_reader = std.io.bufferedReader(stdin.reader());
    const reader = buffered_reader.reader();

    const stdout = std.io.getStdOut();
    var buffered_writer = std.io.bufferedWriter(stdout.writer());
    defer buffered_writer.flush() catch {};
    const writer = buffered_writer.writer();

    const input = try reader.readAllAlloc(allocator, std.math.maxInt(usize));
    var tokens_it = std.mem.tokenizeAny(u8, input, &std.ascii.whitespace);
    const n = try std.fmt.parseInt(u64, tokens_it.next().?, 10);
    const y = try std.fmt.parseInt(u64, tokens_it.next().?, 10);

    for (0..n + 1) |i| {
        for (0..n + 1 - i) |j| {
            const k = n - i - j;
            if (10000 * i + 5000 * j + 1000 * k == y) {
                try writer.print("{} {} {}\n", .{ i, j, k });
                return;
            }
        }
    }

    try writer.print("-1 -1 -1\n", .{});
}
