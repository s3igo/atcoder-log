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
    const as = try allocator.alloc(u64, n);
    for (as) |*a| a.* = try std.fmt.parseInt(u64, tokens_it.next().?, 10);

    var cnt: u64 = 0;
    while (cond: {
        for (as) |a| {
            if (a & 1 == 1) break :cond false;
        } else break :cond true;
    }) : (cnt += 1) {
        for (as) |*a| a.* /= 2;
    }

    try writer.print("{}\n", .{cnt});
}
