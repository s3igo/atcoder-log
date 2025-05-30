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

    const input = try stdin.reader().readAllAlloc(allocator, std.math.maxInt(usize));
    var tokens: Tokens = .init(input);

    const d, const n = tokens.parseN(u64, 2);

    const day_over_day_diffs = try allocator.alloc(i64, d + 1);
    @memset(day_over_day_diffs, 0);
    for (0..n) |_| {
        const l, const r = tokens.parseNVec(u64, 2) - @as(@Vector(2, u64), @splat(1));
        day_over_day_diffs[l] += 1;
        day_over_day_diffs[r + 1] -= 1;
    }

    var prev: i64 = 0;
    for (0..d) |i| {
        prev += day_over_day_diffs[i];
        try writer.print("{d}\n", .{prev});
    }
}

/// Tokenizer and parser for values separated by any whitespace characters.
/// Splits input string into tokens and converts them to numeric types.
///
/// **Features**:
///   - String tokenization based on whitespace delimiters
///   - Parsing tokens into numeric types
///   - Batch parsing of multiple values into arrays, vectors, or slices
///
/// **Example**:
///   ```zig
///   // Parse "123 456" into integers
///   var t1: Tokens = .init("123 456");
///   const a = t1.parse(i32);
///   const b = t1.parse(i32);
///   // a = 123, b = 456
///
///   // Parse "1 2\n3" into an array of integers
///   var t2: Tokens = .init("1 2\n3");
///   const arr = t2.parseN(i32, 3);
///   // arr = [3]i32{ 1, 2, 3 }
///   ```
pub const Tokens = struct {
    iter: std.mem.TokenIterator(u8, .any),

    const Self = @This();

    pub fn init(str: []const u8) Self {
        return .{ .iter = std.mem.tokenizeAny(u8, str, &std.ascii.whitespace) };
    }

    pub fn next(self: *Self) ?[]const u8 {
        return self.iter.next();
    }

    pub fn raw(self: *Self) []const u8 {
        return self.next().?;
    }

    pub fn parse(self: *Self, comptime T: type) T {
        return std.fmt.parseInt(T, self.raw(), 0) catch |err|
            std.debug.panic("Tokens.parse <{s}>: {s}", .{ @typeName(T), @errorName(err) });
    }

    pub fn parseN(self: *Self, comptime T: type, comptime n: comptime_int) [n]T {
        var result: [n]T = undefined;
        for (&result) |*elem| elem.* = self.parse(T);
        return result;
    }

    pub fn parseNVec(self: *Self, comptime T: type, comptime n: comptime_int) @Vector(n, T) {
        return self.parseN(T, n);
    }

    pub fn parseNAlloc(self: *Self, allocator: std.mem.Allocator, comptime T: type, n: usize) ![]T {
        const result = try allocator.alloc(T, n);
        errdefer allocator.free(result);
        for (result) |*elem| elem.* = self.parse(T);
        return result;
    }
};
