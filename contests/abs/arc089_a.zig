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
    var lines_it = std.mem.tokenizeAny(u8, input, std.ascii.whitespace[1..]);
    _ = lines_it.next();
    var txys: std.ArrayList(State) = .init(allocator);
    while (lines_it.next()) |line| {
        var tokens_it = std.mem.tokenizeScalar(u8, line, ' ');
        try txys.append(.{
            try std.fmt.parseInt(i64, tokens_it.next().?, 0),
            try std.fmt.parseInt(i64, tokens_it.next().?, 0),
            try std.fmt.parseInt(i64, tokens_it.next().?, 0),
        });
    }

    var state: State = .{ 0, 0, 0 };
    const cond = for (txys.items) |txy| {
        const t0, const x0, const y0 = state;
        const t, const x, const y = txy;
        const dt = t - t0;
        const dx = @abs(x - x0);
        const dy = @abs(y - y0);
        const d: i64 = @intCast(dx + dy);
        if (d <= dt and dt - d & 1 == 0) state = txy else break false;
    } else true;

    try writer.print("{s}\n", .{if (cond) "Yes" else "No"});
}

const State = struct { i64, i64, i64 };
