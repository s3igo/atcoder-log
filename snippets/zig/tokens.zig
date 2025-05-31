const std = @import("std");

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

test "Tokens - initialization" {
    const testing = std.testing;

    { // Empty string
        var tokens: Tokens = .init("");
        try testing.expectEqual(@as(?[]const u8, null), tokens.next());
    }

    { // Single token
        var tokens: Tokens = .init("123");
        try testing.expectEqualStrings("123", tokens.next().?);
        try testing.expectEqual(@as(?[]const u8, null), tokens.next());
    }

    { // Multiple tokens
        var tokens: Tokens = .init("abc def ghi");
        try testing.expectEqualStrings("abc", tokens.next().?);
        try testing.expectEqualStrings("def", tokens.next().?);
        try testing.expectEqualStrings("ghi", tokens.next().?);
        try testing.expectEqual(@as(?[]const u8, null), tokens.next());
    }

    { // Multiple whitespace characters
        var tokens: Tokens = .init(" a  b\tc\nd\re ");
        try testing.expectEqualStrings("a", tokens.next().?);
        try testing.expectEqualStrings("b", tokens.next().?);
        try testing.expectEqualStrings("c", tokens.next().?);
        try testing.expectEqualStrings("d", tokens.next().?);
        try testing.expectEqualStrings("e", tokens.next().?);
        try testing.expectEqual(@as(?[]const u8, null), tokens.next());
    }
}

test "Tokens - raw method" {
    const testing = std.testing;

    var tokens: Tokens = .init("123 456");
    try testing.expectEqualStrings("123", tokens.raw());
    try testing.expectEqualStrings("456", tokens.raw());

    // Note: We can't test the panic case directly in Zig's testing framework
    // The following would panic:
    // _ = tokens.raw();  // Would panic - no more tokens
}

test "Tokens - tryParse method" {
    const testing = std.testing;

    { // Basic parse test
        var tokens: Tokens = .init("123 -456 0");
        try testing.expectEqual(@as(i32, 123), try tokens.tryParse(i32));
        try testing.expectEqual(@as(i32, -456), try tokens.tryParse(i32));
        try testing.expectEqual(@as(i32, 0), try tokens.tryParse(i32));
    }

    { // Different integer types
        var tokens: Tokens = .init("127 255 32767 65535");
        try testing.expectEqual(@as(i8, 127), try tokens.tryParse(i8));
        try testing.expectEqual(@as(u8, 255), try tokens.tryParse(u8));
        try testing.expectEqual(@as(i16, 32767), try tokens.tryParse(i16));
        try testing.expectEqual(@as(u16, 65535), try tokens.tryParse(u16));
    }

    { // Error: No more tokens
        var tokens: Tokens = .init("10");
        _ = try tokens.tryParse(i32);
        try testing.expectError(error.NoMoreTokens, tokens.tryParse(i32));
    }

    { // Error: Invalid integer format
        var tokens: Tokens = .init("abc");
        try testing.expectError(error.InvalidCharacter, tokens.tryParse(i32));
    }

    { // Error: Integer overflow
        var tokens: Tokens = .init("256");
        try testing.expectError(error.Overflow, tokens.tryParse(u8));
    }
}

test "Tokens - parse method" {
    const testing = std.testing;

    { // Parse integers
        var tokens: Tokens = .init("123 -456 0 9876543210");
        try testing.expectEqual(@as(i32, 123), tokens.parse(i32));
        try testing.expectEqual(@as(i32, -456), tokens.parse(i32));
        try testing.expectEqual(@as(i32, 0), tokens.parse(i32));
        try testing.expectEqual(@as(u64, 9876543210), tokens.parse(u64));
    }

    { // Parse different integer types
        var tokens: Tokens = .init("127 255 32767 65535");
        try testing.expectEqual(@as(i8, 127), tokens.parse(i8));
        try testing.expectEqual(@as(u8, 255), tokens.parse(u8));
        try testing.expectEqual(@as(i16, 32767), tokens.parse(i16));
        try testing.expectEqual(@as(u16, 65535), tokens.parse(u16));
    }

    // Error cases cannot be directly tested as they would panic
    // But the following would panic at runtime:
    // var tokens1: Tokens = .init("256");
    // _ = tokens1.parse(u8);  // Would panic with Overflow
    //
    // var tokens2: Tokens = .init("abc");
    // _ = tokens2.parse(i32);  // Would panic with InvalidCharacter
}

test "Tokens - parseN method" {
    const testing = std.testing;

    { // Parse array of integers
        var tokens: Tokens = .init("10 20 30 40 50");
        const arr = tokens.parseN(i32, 5);
        try testing.expectEqual(@as(i32, 10), arr[0]);
        try testing.expectEqual(@as(i32, 20), arr[1]);
        try testing.expectEqual(@as(i32, 30), arr[2]);
        try testing.expectEqual(@as(i32, 40), arr[3]);
        try testing.expectEqual(@as(i32, 50), arr[4]);
    }

    { // Parse array of different types
        var tokens: Tokens = .init("123 456");
        const arr = tokens.parseN(u16, 2);
        try testing.expectEqual(@as(u16, 123), arr[0]);
        try testing.expectEqual(@as(u16, 456), arr[1]);
    }

    // Error: not enough tokens - this would panic at runtime:
    // var tokens: Tokens = .init("10 20");
    // _ = tokens.parseN(i32, 3);  // Would panic - not enough tokens
}

test "Tokens - parseNVec method" {
    const testing = std.testing;

    { // Parse vector of integers
        var tokens: Tokens = .init("1 2 3 4");
        const vec = tokens.parseNVec(i32, 4);
        const expected: @Vector(4, i32) = .{ 1, 2, 3, 4 };
        try testing.expectEqual(expected, vec);
    }

    { // Vector operations
        var tokens: Tokens = .init("1 2 3 4");
        const vec = tokens.parseNVec(i32, 4);
        const two: @Vector(4, i32) = .{ 2, 2, 2, 2 };
        const result = vec * two;
        const expected: @Vector(4, i32) = .{ 2, 4, 6, 8 };
        try testing.expectEqual(expected, result);
    }
}

test "Tokens - integration" {
    const testing = std.testing;

    // Mixed parsing operations
    const input = "5 10 15 20 25";
    var tokens: Tokens = .init(input);

    // Parse single value
    const first = tokens.parse(i32);
    try testing.expectEqual(@as(i32, 5), first);

    // Parse array
    const arr = tokens.parseN(i32, 2);
    try testing.expectEqual(@as(i32, 10), arr[0]);
    try testing.expectEqual(@as(i32, 15), arr[1]);

    // Parse vector
    const vec = tokens.parseNVec(i32, 2);
    const expected: @Vector(2, i32) = .{ 20, 25 };
    try testing.expectEqual(expected, vec);

    // No more tokens
    try testing.expectEqual(@as(?[]const u8, null), tokens.next());
}

test "Tokens - parseNAlloc method" {
    const testing = std.testing;
    const allocator = testing.allocator;

    { // Parse runtime-length array of integers
        var tokens: Tokens = .init("10 20 30 40 50");
        const n: usize = 5;
        const result = try tokens.parseNAlloc(allocator, i32, n);
        defer allocator.free(result);

        try testing.expectEqual(@as(usize, 5), result.len);
        try testing.expectEqual(@as(i32, 10), result[0]);
        try testing.expectEqual(@as(i32, 20), result[1]);
        try testing.expectEqual(@as(i32, 30), result[2]);
        try testing.expectEqual(@as(i32, 40), result[3]);
        try testing.expectEqual(@as(i32, 50), result[4]);
    }

    { // Varying length based on runtime value
        const tokens: Tokens = .init("1 2 3 4 5 6 7 8 9 10");
        const lengths = [_]usize{ 3, 5, 2 };

        for (lengths) |len| {
            var t_copy = tokens;
            const result = try t_copy.parseNAlloc(allocator, i32, len);
            defer allocator.free(result);

            try testing.expectEqual(len, result.len);
            for (0..len) |i| {
                try testing.expectEqual(@as(i32, @intCast(i + 1)), result[i]);
            }
        }
    }

    { // Zero length
        var tokens: Tokens = .init("1 2 3");
        const result = try tokens.parseNAlloc(allocator, i32, 0);
        defer allocator.free(result);

        try testing.expectEqual(@as(usize, 0), result.len);
    }

    // Error cases:
    // Not enough tokens would panic at runtime
    // tokens.parseNAlloc(allocator, i32, 10) when only 5 tokens available
}

test Tokens {
    const testing = std.testing;

    { // Basic initialization and parsing
        const input = "  10 20\t\n-30  40 ";
        var tokens: Tokens = .init(input);

        // Parse individual values with different types
        try testing.expectEqual(@as(i8, 10), tokens.parse(i8));
        try testing.expectEqual(@as(u8, 20), tokens.parse(u8));
        try testing.expectEqual(@as(i16, -30), tokens.parse(i16));

        // Parse array and check its contents
        const arr = tokens.parseN(i32, 1);
        try testing.expectEqual(@as(i32, 40), arr[0]);

        // No more tokens
        try testing.expectEqual(@as(?[]const u8, null), tokens.next());
    }

    { // Vector parsing and operations
        var tokens: Tokens = .init("1 2 3 4");
        const vec = tokens.parseNVec(i32, 4);

        // Vector operations
        const two: @Vector(4, i32) = @splat(2);
        const result = vec * two;
        const expected: @Vector(4, i32) = .{ 2, 4, 6, 8 };
        try testing.expectEqual(expected, result);
    }

    { // Dynamic allocation with parseNAlloc
        const allocator = testing.allocator;
        var tokens: Tokens = .init("100 200 300");

        // Dynamically allocate based on runtime value
        var len: usize = 3;
        _ = &len;
        const values = try tokens.parseNAlloc(allocator, i32, len);
        defer allocator.free(values);

        try testing.expectEqual(@as(usize, 3), values.len);
        try testing.expectEqual(@as(i32, 100), values[0]);
        try testing.expectEqual(@as(i32, 200), values[1]);
        try testing.expectEqual(@as(i32, 300), values[2]);
    }

    // Error cases cannot be directly tested in Zig's testing framework
    // as they would panic, but these operations would panic at runtime:
    //
    // var t1: Tokens = .init("256");
    // _ = t1.parse(u8);  // Would panic with Overflow
    //
    // var t2: Tokens = .init("abc");
    // _ = t2.parse(i32);  // Would panic with InvalidCharacter
}
