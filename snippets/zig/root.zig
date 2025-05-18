const std = @import("std");

/// Performs run-length encoding on a slice.
/// Returns a newly allocated slice of tuples containing {value, count}.
/// The caller owns the returned memory.
///
/// Example:
///   var arr = [_]i32{ 1, 1, 2, 3, 3, 3, 4, 4, 1 };
///   const encoded = try runLengthEncode(i32, allocator, &arr);
///   defer allocator.free(encoded);
///   // encoded is now [{1, 2}, {2, 1}, {3, 3}, {4, 2}, {1, 1}]
pub fn runLengthEncode(
    comptime T: type,
    allocator: std.mem.Allocator,
    slice: []const T,
) ![]struct { T, usize } {
    if (slice.len == 0) return &.{};

    var runs: usize = 1;
    for (1..slice.len) |i| {
        if (slice[i] != slice[i - 1]) runs += 1;
    }

    const result = try allocator.alloc(struct { T, usize }, runs);
    errdefer allocator.free(result);

    var i: usize = 0;
    var val = slice[0];
    var cnt: usize = 1;
    var out_idx: usize = 0;

    while (i + 1 < slice.len) : (i += 1) {
        if (slice[i + 1] == val) cnt += 1 else {
            result[out_idx] = .{ val, cnt };
            out_idx += 1;
            val = slice[i + 1];
            cnt = 1;
        }
    } else result[out_idx] = .{ val, cnt };

    return result;
}

/// Decodes a run-length encoded slice back to original form.
/// Takes a slice of value-count tuples and returns the expanded data.
///
/// Example:
///   const encoded = [_]struct { i32, usize }{ .{ 1, 2 }, .{ 2, 1 }, .{ 3, 3 } };
///   const decoded = try runLengthDecode(i32, allocator, &encoded);
///   defer allocator.free(decoded);
///   // decoded is now [1, 1, 2, 3, 3, 3]
pub fn runLengthDecode(
    comptime T: type,
    allocator: std.mem.Allocator,
    encoded: []const struct { T, usize },
) ![]T {
    var total: usize = 0;
    for (encoded) |run| total += run[1];

    const result = try allocator.alloc(T, total);
    errdefer allocator.free(result);

    var out_idx: usize = 0;
    for (encoded) |run| {
        const val, const cnt = run;

        var i: usize = 0;
        while (i < cnt) : (i += 1) {
            result[out_idx] = val;
            out_idx += 1;
        }
    }

    return result;
}

test runLengthEncode {
    const testing = std.testing;
    const allocator = testing.allocator;

    { // Basic encoding test
        const data = [_]i32{ 1, 1, 2, 3, 3, 3, 4, 4, 1 };
        const encoded = try runLengthEncode(i32, allocator, &data);
        defer allocator.free(encoded);

        try testing.expectEqual(@as(usize, 5), encoded.len);
        try testing.expectEqual(@as(i32, 1), encoded[0][0]);
        try testing.expectEqual(@as(usize, 2), encoded[0][1]);
        try testing.expectEqual(@as(i32, 2), encoded[1][0]);
        try testing.expectEqual(@as(usize, 1), encoded[1][1]);
        try testing.expectEqual(@as(i32, 3), encoded[2][0]);
        try testing.expectEqual(@as(usize, 3), encoded[2][1]);
        try testing.expectEqual(@as(i32, 4), encoded[3][0]);
        try testing.expectEqual(@as(usize, 2), encoded[3][1]);
        try testing.expectEqual(@as(i32, 1), encoded[4][0]);
        try testing.expectEqual(@as(usize, 1), encoded[4][1]);
    }

    { // Empty array
        const data = [_]i32{};
        const encoded = try runLengthEncode(i32, allocator, &data);
        defer allocator.free(encoded);
        try testing.expectEqual(@as(usize, 0), encoded.len);
    }

    { // Single element
        const data = [_]i32{5};
        const encoded = try runLengthEncode(i32, allocator, &data);
        defer allocator.free(encoded);
        try testing.expectEqual(@as(usize, 1), encoded.len);
        try testing.expectEqual(@as(i32, 5), encoded[0][0]);
        try testing.expectEqual(@as(usize, 1), encoded[0][1]);
    }

    { // All same elements
        const data = [_]i32{ 7, 7, 7, 7 };
        const encoded = try runLengthEncode(i32, allocator, &data);
        defer allocator.free(encoded);
        try testing.expectEqual(@as(usize, 1), encoded.len);
        try testing.expectEqual(@as(i32, 7), encoded[0][0]);
        try testing.expectEqual(@as(usize, 4), encoded[0][1]);
    }
}

test runLengthDecode {
    const testing = std.testing;
    const allocator = testing.allocator;

    { // Basic decoding test
        const data = [_]struct { i32, usize }{
            .{ 1, 2 },
            .{ 2, 1 },
            .{ 3, 3 },
            .{ 4, 2 },
            .{ 1, 1 },
        };
        const expected = [_]i32{ 1, 1, 2, 3, 3, 3, 4, 4, 1 };

        const decoded = try runLengthDecode(i32, allocator, &data);
        defer allocator.free(decoded);

        try testing.expectEqualSlices(i32, &expected, decoded);
    }

    { // Empty array
        const data = [_]struct { i32, usize }{};
        const decoded = try runLengthDecode(i32, allocator, &data);
        defer allocator.free(decoded);
        try testing.expectEqual(@as(usize, 0), decoded.len);
    }

    { // Single element
        const data = [_]struct { i32, usize }{.{ 42, 1 }};
        const expected = [_]i32{42};

        const decoded = try runLengthDecode(i32, allocator, &data);
        defer allocator.free(decoded);

        try testing.expectEqualSlices(i32, &expected, decoded);
    }

    { // Single value repeated
        const data = [_]struct { i32, usize }{.{ 7, 5 }};
        const expected = [_]i32{ 7, 7, 7, 7, 7 };

        const decoded = try runLengthDecode(i32, allocator, &data);
        defer allocator.free(decoded);

        try testing.expectEqualSlices(i32, &expected, decoded);
    }

    { // Round-trip test (encode then decode)
        const original = [_]i32{ 1, 1, 2, 3, 3, 3, 4, 4, 1 };

        const encoded = try runLengthEncode(i32, allocator, &original);
        defer allocator.free(encoded);

        const decoded = try runLengthDecode(i32, allocator, encoded);
        defer allocator.free(decoded);

        try testing.expectEqualSlices(i32, &original, decoded);
    }
}
