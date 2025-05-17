const std = @import("std");

pub fn main() !void {
    var arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = arena.allocator();
    _ = &allocator; // Suppress error: local variable is never mutate

    const stdin = std.io.getStdIn();
    var buffered_reader = std.io.bufferedReader(stdin.reader());
    var reader = buffered_reader.reader();
    _ = &reader; // Ditto

    const stdout = std.io.getStdOut();
    var buffered_writer = std.io.bufferedWriter(stdout.writer());
    defer buffered_writer.flush() catch unreachable;
    var writer = buffered_writer.writer();
    _ = &writer; // Ditto

    const input = try reader.readAllAlloc(allocator, std.math.maxInt(usize));
    var tokens_it = std.mem.tokenizeAny(u8, input, &std.ascii.whitespace);
    var tokens: std.ArrayList(u64) = .init(allocator);
    while (tokens_it.next()) |token| {
        try tokens.append(try std.fmt.parseInt(u64, token, 0));
    }
    const n, const a, const b = tokens.items[0..3].*;

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
