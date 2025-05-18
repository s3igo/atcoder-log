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
    const a = try std.fmt.parseInt(u64, tokens_it.next().?, 10);
    const b = try std.fmt.parseInt(u64, tokens_it.next().?, 10);

    var sum: u64 = 0;
    for (1..n + 1) |i| {
        if (cond: {
            var div = i;
            var digits_sum: u64 = 0;
            while (div != 0) : (div /= 10) digits_sum += div % 10;
            break :cond a <= digits_sum and digits_sum <= b;
        }) sum += i;
    }

    try writer.print("{}\n", .{sum});
}
