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
    const a = try std.fmt.parseInt(u64, input.next().?, 10);
    const b = try std.fmt.parseInt(u64, input.next().?, 10);

    var sum: u64 = 0;
    for (1..n + 1) |i| {
        if (cond: {
            var div = i;
            var digits_sum: u64 = 0;
            while (div != 0) : (div /= 10) digits_sum += div % 10;
            break :cond a <= digits_sum and digits_sum <= b;
        }) sum += i;
    }

    try writer.print("{d}\n", .{sum});
}
