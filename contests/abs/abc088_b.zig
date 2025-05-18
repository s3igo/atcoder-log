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

    std.mem.sortUnstable(u64, as, {}, std.sort.desc(u64));

    var ans: u64 = 0;
    for (as, 0..) |a, i| {
        if (i & 1 == 0) ans += a else ans -= a;
    }

    try writer.print("{}\n", .{ans});
}
