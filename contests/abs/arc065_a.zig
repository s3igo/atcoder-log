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
    const line = std.mem.trimRight(u8, raw_input, &std.ascii.whitespace);

    var s = try allocator.dupe(u8, line);

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
