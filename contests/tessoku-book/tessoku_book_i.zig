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

    const h, const w, const n = tokens.parseN(u64, 3);

    const cumsum: Tensor(i64) = try .init(allocator, &.{ h, w });

    for (0..n) |_| {
        const a, const b, const c, const d = tokens.parseNVec(u64, 4) - @as(@Vector(4, u64), @splat(1));
        cumsum.at(&.{ a, b }).* += 1;
        cumsum.at(&.{ a, d + 1 }).* -= 1;
        cumsum.at(&.{ c + 1, b }).* -= 1;
        cumsum.at(&.{ c + 1, d + 1 }).* += 1;
    }

    for (0..h) |i| {
        for (0..w) |j| {
            const prev = cumsum.getOrDefault(&.{ i, j -% 1 }, 0);
            cumsum.at(&.{ i, j }).* += prev;
        }
    }

    for (0..w) |i| {
        for (0..h) |j| {
            const prev = cumsum.getOrDefault(&.{ i -% 1, j }, 0);
            cumsum.at(&.{ i, j }).* += prev;
        }
    }

    for (0..h) |i| {
        for (0..w) |j| {
            try writer.print("{d}", .{cumsum.at(&.{ i, j }).*});
            try writer.print("{s}", .{if (j < w - 1) " " else "\n"});
        }
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
        return self.next() orelse std.debug.panic("Tokens.raw(): No more tokens", .{});
    }

    pub fn tryParse(self: *Self, comptime T: type) !T {
        const token = self.next() orelse return error.NoMoreTokens;
        return std.fmt.parseInt(T, token, 0);
    }

    pub fn parse(self: *Self, comptime T: type) T {
        return self.tryParse(T) catch |err|
            std.debug.panic("Tokens.parse({s}): {s}", .{ @typeName(T), @errorName(err) });
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
        for (result) |*elem| elem.* = try self.tryParse(T);
        return result;
    }
};

/// Generic implementation of a multidimensional array tensor.
/// Provides multidimensional arrays for any type T with runtime-known sizes and
/// dimensions.
///
/// **Features**:
///   - Supports any type (primitives, structs, tuples)
///   - Arbitrary dimensions (from 0-dimensional scalars to multi-dimensional)
///   - Zero-initialized memory allocation
///   - Safe index boundary checks
///
/// **Example**:
///   ```zig
///   // Create a 2x3 floating point tensor
///   const dims = [_]usize{ 2, 3 };
///   var tensor: Tensor(f32) = try .init(allocator, &dims);
///   defer tensor.deinit();
///
///   // Access elements
///   tensor.at(&.{ 0, 1 }).* = 3.14;
///   const value = tensor.at(&.{ 0, 1 }).*;
///
///   // Access elements (with error handling)
///   (try tensor.tryAt(&.{ 0, 1 })).* = 3.14;
///   const value_try = (try tensor.tryAt(&.{ 0, 1 })).*;
///
///   // Get value with default fallback
///   const value_or_default = tensor.getOrDefault(&.{ 0, 1 }, 0.0);
///
///   // Tensor with struct elements
///   const Point = struct { x: f32, y: f32 };
///   var points_tensor: Tensor(Point) = try .init(allocator, &.{10, 10});
///   defer points_tensor.deinit();
///   ```
pub fn Tensor(comptime T: type) type {
    return struct {
        data: []T,
        dimensions: []usize,
        allocator: std.mem.Allocator,

        const Self = @This();

        pub fn init(allocator: std.mem.Allocator, dimensions: []const usize) !Self {
            const dims_copy = try allocator.dupe(usize, dimensions);
            errdefer allocator.free(dims_copy);

            var total_size: usize = 1;
            for (dimensions) |dim| total_size *= dim;

            const data = try allocator.alloc(T, total_size);
            errdefer allocator.free(data);

            // Initialize with zeros following common tensor computation practice
            @memset(data, std.mem.zeroes(T));

            return .{
                .data = data,
                .dimensions = dims_copy,
                .allocator = allocator,
            };
        }

        pub fn deinit(self: Self) void {
            self.allocator.free(self.data);
            self.allocator.free(self.dimensions);
        }

        fn index(self: Self, indices: []const usize) !usize {
            if (indices.len != self.dimensions.len) return error.DimensionMismatch;

            var idx: usize = 0;
            var stride: usize = 1;

            // Calculate from the last dimension (row-major order)
            var i: usize = self.dimensions.len;
            while (i > 0) : ({
                idx += indices[i] * stride;
                stride *= self.dimensions[i];
            }) {
                i -= 1;
                if (indices[i] >= self.dimensions[i]) return error.IndexOutOfBounds;
            }

            return idx;
        }

        pub fn tryAt(self: Self, indices: []const usize) !*T {
            const idx = try self.index(indices);
            return &self.data[idx];
        }

        pub fn at(self: Self, indices: []const usize) *T {
            return self.tryAt(indices) catch |err|
                std.debug.panic("Tensor({s}).at({any}): {s}", .{
                    @typeName(T),
                    indices,
                    @errorName(err),
                });
        }

        pub fn getOrDefault(self: Self, indices: []const usize, default: T) T {
            const idx = self.index(indices) catch return default;
            return self.data[idx];
        }

        pub fn rank(self: Self) usize {
            return self.dimensions.len;
        }

        pub fn dimSize(self: Self, dim: usize) !usize {
            if (dim >= self.dimensions.len) return error.DimensionOutOfBounds;
            return self.dimensions[dim];
        }

        pub fn size(self: Self) usize {
            var total: usize = 1;
            for (self.dimensions) |dim| total *= dim;
            return total;
        }
    };
}
