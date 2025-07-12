const std = @import("std");
const Allocator = std.mem.Allocator;
const tomboy = @import("../../../../tomboy.zig");
const Self = @This();

target: tomboy.Target,

pub fn serialize(self: *Self, writer: *tomboy.ir.binary.Writer) !void {
    try writer.begin(.object, @typeName(Self));

    try self.target.call(.serialize, .{writer});
    return try writer.end();
}

pub fn deinit(self: *Self, _: Allocator) void {
    self.target.unref();
}
