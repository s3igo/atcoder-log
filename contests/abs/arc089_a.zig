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
    const txys = try allocator.alloc(Step, n);
    for (txys) |*txy| txy.* = .{
        try std.fmt.parseInt(i64, tokens_it.next().?, 10),
        try std.fmt.parseInt(i64, tokens_it.next().?, 10),
        try std.fmt.parseInt(i64, tokens_it.next().?, 10),
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
