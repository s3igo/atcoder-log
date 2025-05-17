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
    const deduped = try runLengthEncode(u64, allocator, ds.items);

    try writer.print("{}\n", .{deduped.len});
}

pub fn runLengthEncode(
    comptime T: type,
    allocator: std.mem.Allocator,
    slice: []const T,
) ![]struct { T, usize } {
    if (slice.len == 0) return &.{};

    var runs: usize = 1;
    for (1..slice.len) |i| {
        if (slice[i] != slice[i - 1]) runs += 1;
    }

    const result = try allocator.alloc(struct { T, usize }, runs);
    errdefer allocator.free(result);

    var i: usize = 0;
    var val = slice[0];
    var cnt: usize = 1;
    var out_idx: usize = 0;

    while (i + 1 < slice.len) : (i += 1) {
        if (slice[i + 1] == val) cnt += 1 else {
            result[out_idx] = .{ val, cnt };
            out_idx += 1;
            val = slice[i + 1];
            cnt = 1;
        }
    }

    result[out_idx] = .{ val, cnt };

    return result;
}
