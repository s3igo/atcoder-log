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

    const d = try std.fmt.parseInt(u64, input.next().?, 10);
    const n = try std.fmt.parseInt(u64, input.next().?, 10);

    const day_over_day_diffs = try allocator.alloc(i64, d + 1);
    @memset(day_over_day_diffs, 0);
    for (0..n) |_| {
        const l = try std.fmt.parseInt(u64, input.next().?, 10) - 1;
        const r = try std.fmt.parseInt(u64, input.next().?, 10) - 1;
        day_over_day_diffs[l] += 1;
        day_over_day_diffs[r + 1] -= 1;
    }

    var prev: i64 = 0;
    for (0..d) |i| {
        prev += day_over_day_diffs[i];
        try writer.print("{d}\n", .{prev});
    }
}
