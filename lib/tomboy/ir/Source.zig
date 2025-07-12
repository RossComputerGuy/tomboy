const std = @import("std");
const Allocator = std.mem.Allocator;
const Self = @This();

path: ?[]const u8,
line: usize,
column: usize,

pub fn deinit(self: *Self, alloc: Allocator) void {
    if (self.path) |path| alloc.free(path);
}

pub fn format(self: *Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    return writer.print("{s}:{d}.{d}", .{ self.path orelse "", self.line, self.column });
}
