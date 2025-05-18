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
    const ds = try allocator.alloc(u64, n);
    for (ds) |*d| d.* = try std.fmt.parseInt(u64, tokens_it.next().?, 10);

    std.mem.sortUnstable(u64, ds, {}, std.sort.asc(u64));

    try writer.print("{}\n", .{dedupLen(u64, ds)});
}

/// Returns the length of a slice after removing consecutive duplicates.
/// The input slice is modified in place to remove consecutive duplicates.
///
/// **Example**:
///   ```zig
///   var arr = [_]i32{ 1, 1, 2, 3, 3, 3, 4, 4, 1 };
///   const len = dedupLen(i32, &arr);
///   // len is 5
///   ```
fn dedupLen(comptime T: type, slice: []T) usize {
    if (slice.len <= 1) return slice.len;

    var write_idx: usize = 1;
    for (1..slice.len) |i| {
        if (slice[i] != slice[i - 1]) {
            slice[write_idx] = slice[i];
            write_idx += 1;
        }
    }

    return write_idx;
}
