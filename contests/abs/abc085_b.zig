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
    _ = tokens_it.next();
    var ds: std.ArrayList(u64) = .init(allocator);
    while (tokens_it.next()) |token| {
        try ds.append(try std.fmt.parseInt(u64, token, 0));
    }

    std.mem.sortUnstable(u64, ds.items, {}, std.sort.asc(u64));
    const deduped = compact(u64, ds.items);

    try writer.print("{}\n", .{deduped.len});
}

/// NOTE: Modifies the input slice
fn compact(comptime T: type, slice: []T) []T {
    if (slice.len <= 1) return slice;

    var writeIdx: usize = 1;
    for (1..slice.len) |i| {
        if (slice[i] != slice[i - 1]) {
            slice[writeIdx] = slice[i];
            writeIdx += 1;
        }
    }

    return slice[0..writeIdx];
}
