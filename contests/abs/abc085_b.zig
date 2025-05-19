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
    const ds = try allocator.alloc(u64, n);
    for (ds) |*d| d.* = try std.fmt.parseInt(u64, input.next().?, 10);

    std.mem.sortUnstable(u64, ds, {}, std.sort.asc(u64));

    try writer.print("{d}\n", .{dedupLen(u64, ds)});
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
