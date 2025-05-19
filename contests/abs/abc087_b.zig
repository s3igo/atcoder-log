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

    const a = try std.fmt.parseInt(u64, input.next().?, 10);
    const b = try std.fmt.parseInt(u64, input.next().?, 10);
    const c = try std.fmt.parseInt(u64, input.next().?, 10);
    const x = try std.fmt.parseInt(u64, input.next().?, 10);

    var cnt: u64 = 0;
    for (0..a + 1) |i| {
        for (0..b + 1) |j| {
            for (0..c + 1) |k| {
                const sum = 500 * i + 100 * j + 50 * k;
                if (sum == x) cnt += 1;
            }
        }
    }

    try writer.print("{d}\n", .{cnt});
}
