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
    const k = try std.fmt.parseInt(u64, input.next().?, 10);

    var cnt: u64 = 0;
    for (1..n + 1) |x| {
        for (1..n + 1) |y| {
            const z = k -| x -| y;
            if (1 <= z and z <= n) cnt += 1;
        }
    }

    try writer.print("{d}\n", .{cnt});
}
