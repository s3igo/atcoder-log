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
    const a = try std.fmt.parseInt(u64, tokens_it.next().?, 10);
    const b = try std.fmt.parseInt(u64, tokens_it.next().?, 10);
    const c = try std.fmt.parseInt(u64, tokens_it.next().?, 10);
    const x = try std.fmt.parseInt(u64, tokens_it.next().?, 10);

    var cnt: u64 = 0;
    for (0..a + 1) |i| {
        for (0..b + 1) |j| {
            for (0..c + 1) |k| {
                const sum = 500 * i + 100 * j + 50 * k;
                if (sum == x) cnt += 1;
            }
        }
    }

    try writer.print("{}\n", .{cnt});
}
