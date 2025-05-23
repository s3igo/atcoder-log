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
    const q = try std.fmt.parseInt(u64, input.next().?, 10);

    const cumsum = try allocator.alloc(u64, n + 1);
    cumsum[0] = 0;
    for (1..n + 1) |i| {
        const a = try std.fmt.parseInt(u64, input.next().?, 10);
        cumsum[i] = cumsum[i - 1] + a;
    }

    for (0..q) |_| {
        const l = try std.fmt.parseInt(u64, input.next().?, 10);
        const r = try std.fmt.parseInt(u64, input.next().?, 10);
        try writer.print("{d}\n", .{cumsum[r] - cumsum[l - 1]});
    }
}
