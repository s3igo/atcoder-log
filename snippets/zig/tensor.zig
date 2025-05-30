const std = @import("std");

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

        pub fn tryAt(self: Self, indices: []const usize) !*T {
            const idx = try self.index(indices);
            return &self.data[idx];
        }

        pub fn at(self: Self, indices: []const usize) *T {
            return self.tryAt(indices) catch |err|
                std.debug.panic("Tensor({s}).at: {s}", .{ @typeName(T), @errorName(err) });
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

test "Tensor - initialization" {
    const testing = std.testing;
    const allocator = testing.allocator;

    { // 2D tensor (2x3) initialization test
        const dims2d = [_]usize{ 2, 3 };
        var tensor2d: Tensor(f32) = try .init(allocator, &dims2d);
        defer tensor2d.deinit();

        // Verify dimensions and size
        try testing.expectEqual(@as(usize, 2), tensor2d.rank());
        try testing.expectEqual(@as(usize, 2), try tensor2d.dimSize(0));
        try testing.expectEqual(@as(usize, 3), try tensor2d.dimSize(1));
        try testing.expectEqual(@as(usize, 6), tensor2d.size()); // 2 * 3 = 6

        // Floating point types should initialize to 0.0
        try testing.expectEqual(@as(f32, 0.0), tensor2d.at(&.{ 0, 0 }).*);
        try testing.expectEqual(@as(f32, 0.0), tensor2d.at(&.{ 0, 1 }).*);
        try testing.expectEqual(@as(f32, 0.0), tensor2d.at(&.{ 1, 2 }).*);
    }

    { // 3D tensor (2x3x4) initialization test
        const dims3d = [_]usize{ 2, 3, 4 };
        var tensor3d: Tensor(i32) = try .init(allocator, &dims3d);
        defer tensor3d.deinit();

        try testing.expectEqual(@as(usize, 3), tensor3d.rank());
        try testing.expectEqual(@as(usize, 2), try tensor3d.dimSize(0));
        try testing.expectEqual(@as(usize, 3), try tensor3d.dimSize(1));
        try testing.expectEqual(@as(usize, 4), try tensor3d.dimSize(2));
        try testing.expectEqual(@as(usize, 24), tensor3d.size()); // 2 * 3 * 4 = 24

        // Integer types should initialize to 0
        try testing.expectEqual(@as(i32, 0), tensor3d.at(&.{ 0, 0, 0 }).*);
        try testing.expectEqual(@as(i32, 0), tensor3d.at(&.{ 0, 0, 1 }).*);
        try testing.expectEqual(@as(i32, 0), tensor3d.at(&.{ 0, 1, 0 }).*);
        try testing.expectEqual(@as(i32, 0), tensor3d.at(&.{ 1, 2, 3 }).*);
    }

    { // Tuple type tensor initialization test
        const dims = [_]usize{ 2, 2 };
        var tensor_tuple: Tensor(struct { f32, i32 }) = try .init(allocator, &dims);
        defer tensor_tuple.deinit();

        try testing.expectEqual(@as(usize, 2), tensor_tuple.rank());
        try testing.expectEqual(@as(usize, 4), tensor_tuple.size()); // 2 * 2 = 4

        // Tuples should initialize to zeroes
        try testing.expectEqualDeep(.{ 0.0, 0 }, tensor_tuple.at(&.{ 0, 0 }).*);
        try testing.expectEqualDeep(.{ 0.0, 0 }, tensor_tuple.at(&.{ 1, 1 }).*);
    }

    { // Struct type tensor initialization test
        const Point = struct { x: f32, y: f32 };

        const dims = [_]usize{ 3, 2 };
        var tensor_struct: Tensor(Point) = try .init(allocator, &dims);
        defer tensor_struct.deinit();

        try testing.expectEqual(@as(usize, 2), tensor_struct.rank());
        try testing.expectEqual(@as(usize, 6), tensor_struct.size()); // 3 * 2 = 6

        // Struct should initialize to zeros
        const expected_point = Point{ .x = 0.0, .y = 0.0 };
        try testing.expectEqual(expected_point.x, tensor_struct.at(&.{ 0, 0 }).*.x);
        try testing.expectEqual(expected_point.y, tensor_struct.at(&.{ 0, 0 }).*.y);
    }
}

test "Tensor - get and set values" {
    const testing = std.testing;
    const allocator = testing.allocator;

    { // Setting and getting values in a 2D tensor
        const dims2d = [_]usize{ 3, 4 };
        var tensor2d: Tensor(f64) = try .init(allocator, &dims2d);
        defer tensor2d.deinit();

        // Set some values
        const indices1 = [_]usize{ 0, 0 };
        const indices2 = [_]usize{ 1, 2 };
        const indices3 = [_]usize{ 2, 3 };

        tensor2d.at(&indices1).* = 1.5;
        tensor2d.at(&indices2).* = 42.0;
        tensor2d.at(&indices3).* = -10.25;

        // Get and verify values
        try testing.expectEqual(@as(f64, 1.5), tensor2d.at(&indices1).*);
        try testing.expectEqual(@as(f64, 42.0), tensor2d.at(&indices2).*);
        try testing.expectEqual(@as(f64, -10.25), tensor2d.at(&indices3).*);

        // Unset values should be the default value of 0
        const unset_indices = [_]usize{ 1, 1 };
        try testing.expectEqual(@as(f64, 0.0), tensor2d.at(&unset_indices).*);
    }

    { // Setting and getting values in a 3D tensor
        const dims3d = [_]usize{ 2, 2, 2 };
        var tensor3d: Tensor(i32) = try .init(allocator, &dims3d);
        defer tensor3d.deinit();

        const indices3d1 = [_]usize{ 0, 0, 1 };
        const indices3d2 = [_]usize{ 1, 1, 1 };

        tensor3d.at(&indices3d1).* = 100;
        tensor3d.at(&indices3d2).* = -50;

        try testing.expectEqual(@as(i32, 100), tensor3d.at(&indices3d1).*);
        try testing.expectEqual(@as(i32, -50), tensor3d.at(&indices3d2).*);
    }
}

test "Tensor - error handling" {
    const testing = std.testing;
    const allocator = testing.allocator;

    { // 2D tensor for error testing
        const dims = [_]usize{ 2, 3 };
        var tensor: Tensor(f32) = try .init(allocator, &dims);
        defer tensor.deinit();

        // Error when index dimension count doesn't match
        const too_few_indices = [_]usize{1};
        const too_many_indices = [_]usize{ 1, 1, 1 };

        try testing.expectError(error.DimensionMismatch, tensor.tryAt(&too_few_indices));
        try testing.expectError(error.DimensionMismatch, tensor.tryAt(&too_many_indices));

        // Error when index is out of bounds
        const out_of_bounds_indices1 = [_]usize{ 2, 0 }; // First dimension only has 0, 1
        const out_of_bounds_indices2 = [_]usize{ 0, 3 }; // Second dimension only has 0, 1, 2

        try testing.expectError(error.IndexOutOfBounds, tensor.tryAt(&out_of_bounds_indices1));
        try testing.expectError(error.IndexOutOfBounds, tensor.tryAt(&out_of_bounds_indices2));

        // Error when requesting dimension size out of bounds
        try testing.expectError(error.DimensionOutOfBounds, tensor.dimSize(2)); // Dimensions are only 0, 1
    }
}

test "Tensor - various types" {
    const testing = std.testing;
    const allocator = testing.allocator;

    { // Testing tensors with various types
        const dims = [_]usize{ 2, 2 };

        // Initialize tensors with different types
        var tensor_i8: Tensor(i8) = try .init(allocator, &dims);
        defer tensor_i8.deinit();

        var tensor_u32: Tensor(u32) = try .init(allocator, &dims);
        defer tensor_u32.deinit();

        var tensor_bool: Tensor(bool) = try .init(allocator, &dims);
        defer tensor_bool.deinit();

        // Set and get values
        const indices = [_]usize{ 0, 1 };

        tensor_i8.at(&indices).* = -5;
        try testing.expectEqual(@as(i8, -5), tensor_i8.at(&indices).*);

        tensor_u32.at(&indices).* = 12345678;
        try testing.expectEqual(@as(u32, 12345678), tensor_u32.at(&indices).*);

        tensor_bool.at(&indices).* = true;
        try testing.expectEqual(true, tensor_bool.at(&indices).*);
    }
}

test "Tensor - edge cases" {
    const testing = std.testing;
    const allocator = testing.allocator;

    { // 1D tensor (vector)
        const dims1d = [_]usize{5};
        var tensor1d: Tensor(f32) = try .init(allocator, &dims1d);
        defer tensor1d.deinit();

        try testing.expectEqual(@as(usize, 1), tensor1d.rank());
        try testing.expectEqual(@as(usize, 5), tensor1d.size());

        const index1d = [_]usize{3};
        tensor1d.at(&index1d).* = 3.14;
        try testing.expectEqual(@as(f32, 3.14), tensor1d.at(&index1d).*);
    }

    { // 0D tensor (scalar)
        const dims0d = [_]usize{};
        var tensor0d: Tensor(i32) = try .init(allocator, &dims0d);
        defer tensor0d.deinit();

        try testing.expectEqual(@as(usize, 0), tensor0d.rank());
        try testing.expectEqual(@as(usize, 1), tensor0d.size()); // Even with 0 dimensions, there's still 1 element

        const index0d = [_]usize{};
        tensor0d.at(&index0d).* = 42;
        try testing.expectEqual(@as(i32, 42), tensor0d.at(&index0d).*);
    }
}

test "Tensor - at" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // 2x3 tensor for testing at
    const dims = [_]usize{ 2, 3 };
    var tensor = try Tensor(f32).init(allocator, &dims);
    defer tensor.deinit();

    // Set a value using at
    tensor.at(&.{ 1, 2 }).* = 3.14;

    // Verify the value using both methods
    try testing.expectEqual(@as(f32, 3.14), (try tensor.tryAt(&.{ 1, 2 })).*);
    try testing.expectEqual(@as(f32, 3.14), tensor.at(&.{ 1, 2 }).*);

    // Verify other positions are still zero
    try testing.expectEqual(@as(f32, 0.0), tensor.at(&.{ 0, 0 }).*);
    try testing.expectEqual(@as(f32, 0.0), tensor.at(&.{ 1, 0 }).*);
}

test "Tensor - getOrDefault" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // 2x3 tensor for testing getOrDefault
    const dims = [_]usize{ 2, 3 };
    var tensor = try Tensor(f32).init(allocator, &dims);
    defer tensor.deinit();

    // Set a value
    tensor.at(&.{ 1, 1 }).* = 42.0;

    // Get valid value with getOrDefault (should return actual value)
    try testing.expectEqual(@as(f32, 42.0), tensor.getOrDefault(&.{ 1, 1 }, 99.9));
    try testing.expectEqual(@as(f32, 0.0), tensor.getOrDefault(&.{ 0, 0 }, 99.9));

    // Test with out of bounds indices (should return default)
    try testing.expectEqual(@as(f32, 99.9), tensor.getOrDefault(&.{ 5, 5 }, 99.9));

    // Test with dimension mismatch (should return default)
    try testing.expectEqual(@as(f32, -1.0), tensor.getOrDefault(&.{1}, -1.0));
    try testing.expectEqual(@as(f32, -2.0), tensor.getOrDefault(&.{ 1, 1, 1 }, -2.0));
}

// Note: We cannot directly test for panics in Zig 0.14.1's standard testing framework
// The implementation of at is designed to panic in these cases:
// - When indices.len != dimensions.len (DimensionMismatch)
// - When any index is out of bounds (IndexOutOfBounds)
//
// Example panic situations:
// ```
// var tensor = try Tensor(f32).init(allocator, &[_]usize{2, 3});
// defer tensor.deinit();
//
// _ = tensor.at(&[_]usize{1}); // Panics: too few indices
// _ = tensor.at(&[_]usize{1, 1, 1}); // Panics: too many indices
// _ = tensor.at(&[_]usize{2, 0}); // Panics: first index out of bounds
// _ = tensor.at(&[_]usize{0, 3}); // Panics: second index out of bounds
// ```

test Tensor {
    const testing = std.testing;
    const allocator = testing.allocator;

    // 3x4 2D tensor (matrix)
    const dims = [_]usize{ 3, 4 };
    var tensor = try Tensor(f32).init(allocator, &dims);
    defer tensor.deinit();

    // Check dimensions
    try testing.expectEqual(@as(usize, 2), tensor.rank());
    try testing.expectEqual(@as(usize, 3), try tensor.dimSize(0));
    try testing.expectEqual(@as(usize, 4), try tensor.dimSize(1));
    try testing.expectEqual(@as(usize, 12), tensor.size()); // 3 * 4 = 12

    // Set values
    tensor.at(&.{ 1, 2 }).* = 3.14;
    tensor.at(&.{ 2, 3 }).* = -1.5;

    // Get and verify values
    try testing.expectEqual(@as(f32, 3.14), tensor.at(&.{ 1, 2 }).*);
    try testing.expectEqual(@as(f32, -1.5), tensor.at(&.{ 2, 3 }).*);
    try testing.expectEqual(@as(f32, 0.0), tensor.at(&.{ 0, 0 }).*); // Initial value is 0

    { // Error handling
        // Dimension mismatch
        try testing.expectError(error.DimensionMismatch, tensor.tryAt(&.{1}));
        try testing.expectError(error.DimensionMismatch, tensor.tryAt(&.{ 0, 1, 2 }));

        // Index out of bounds
        try testing.expectError(error.IndexOutOfBounds, tensor.tryAt(&.{ 3, 0 }));
        try testing.expectError(error.IndexOutOfBounds, tensor.tryAt(&.{ 0, 4 }));

        // Dimension info out of bounds
        try testing.expectError(error.DimensionOutOfBounds, tensor.dimSize(2));
    }
}
