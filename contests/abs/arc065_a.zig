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
    var s = try allocator.dupe(u8, std.mem.trimRight(u8, input, &std.ascii.whitespace));

    std.mem.reverse(u8, s);

    const pats = comptime init: {
        var result: @TypeOf(patterns) = undefined;
        for (patterns, 0..) |pattern, i| {
            var buf: [pattern.len]u8 = pattern[0..pattern.len].*;

            std.mem.reverse(u8, &buf);

            const const_buf: [pattern.len]u8 = buf;
            result[i] = &const_buf;
        }
        break :init result;
    };

    var idx: u64 = 0;
    const cond = while (idx < s.len) {
        for (pats) |pat| {
            if (idx + pat.len <= s.len and std.mem.eql(u8, s[idx .. idx + pat.len], pat)) {
                idx += pat.len;
                break;
            }
        } else break false;
    } else true;

    try writer.print("{s}\n", .{if (cond) "YES" else "NO"});
}

const patterns = [_][]const u8{ "dream", "dreamer", "erase", "eraser" };
