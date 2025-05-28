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

    const h = try std.fmt.parseInt(u64, input.next().?, 10);
    const w = try std.fmt.parseInt(u64, input.next().?, 10);

    const cumsum: Tensor(u64) = try .init(allocator, &.{ h, w });

    for (0..h) |i| {
        for (0..w) |j| {
            const prev = cumsum.getOrDefault(&.{ i, j - 1 }, 0);
            const x = try std.fmt.parseInt(u64, input.next().?, 10);
            cumsum.atAssume(&.{ i, j }).* = prev + x;
        }
    }

    for (0..w) |i| {
        for (0..h) |j| {
            const prev = cumsum.getOrDefault(&.{ i - 1, j }, 0);
            cumsum.atAssume(&.{ i, j }).* += prev;
        }
    }

    const q = try std.fmt.parseInt(u64, input.next().?, 10);

    for (0..q) |_| {
        const a = try std.fmt.parseInt(u64, input.next().?, 10) - 1;
        const b = try std.fmt.parseInt(u64, input.next().?, 10) - 1;
        const c = try std.fmt.parseInt(u64, input.next().?, 10) - 1;
        const d = try std.fmt.parseInt(u64, input.next().?, 10) - 1;
        const tl = cumsum.getOrDefault(&.{ a - 1, b - 1 }, 0);
        const tr = cumsum.getOrDefault(&.{ a - 1, d }, 0);
        const bl = cumsum.getOrDefault(&.{ c, b - 1 }, 0);
        const br = cumsum.getOrDefault(&.{ c, d }, 0);
        try writer.print("{d}\n", .{br + tl - tr - bl});
    }
}

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
///   (try tensor.at(&.{ 0, 1 })).* = 3.14;
///   const value = (try tensor.at(&.{ 0, 1 })).*;
///
///   // Access elements (with panic on error)
///   tensor.atAssume(&.{ 0, 1 }).* = 3.14;
///   const value_assume = tensor.atAssume(&.{ 0, 1 }).*;
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

            // Initialize with zeros following tensor computation conventions
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

        pub fn at(self: Self, indices: []const usize) !*T {
            const idx = try self.index(indices);
            return &self.data[idx];
        }

        pub fn atAssume(self: Self, indices: []const usize) *T {
            return self.at(indices) catch |err|
                std.debug.panic("Tensor({s}).atAssume: {s}", .{ @typeName(T), @errorName(err) });
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
