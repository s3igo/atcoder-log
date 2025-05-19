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
    const txys = try allocator.alloc(Step, n);
    for (txys) |*txy| txy.* = .{
        try std.fmt.parseInt(i64, input.next().?, 10),
        try std.fmt.parseInt(i64, input.next().?, 10),
        try std.fmt.parseInt(i64, input.next().?, 10),
    };

    var acc: Step = .{ 0, 0, 0 };
    const cond = for (txys) |txy| {
        const t0, const x0, const y0 = acc;
        const t, const x, const y = txy;
        const dt = t - t0;
        const dx = @abs(x - x0);
        const dy = @abs(y - y0);
        const d: i64 = @intCast(dx + dy);
        if (d <= dt and dt - d & 1 == 0) acc = txy else break false;
    } else true;

    try writer.print("{s}\n", .{if (cond) "Yes" else "No"});
}

const Step = struct { i64, i64, i64 };
