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
    const tokens_it = std.mem.tokenizeAny(u8, input, "\n ");
    var tokens = try @import("ziter").collect(tokens_it, std.ArrayList([]const u8).init(allocator));

    var sum: usize = 0;
    for (tokens.items[0..3]) |token| {
        sum += try std.fmt.parseInt(usize, token, 10);
    }

    try writer.print("{d} {s}\n", .{ sum, tokens.items[3] });
}
