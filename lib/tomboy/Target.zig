const std = @import("std");
const Allocator = std.mem.Allocator;
const tomboy = @import("../tomboy.zig");
const types = @import("types.zig");
const Self = @This();

pub const Cpu = types.Object(@import("Target/Cpu.zig"));

pub const Os = enum(u8) {
    linux,

    pub fn serialize(self: *Os, writer: *tomboy.ir.binary.Writer) !void {
        try writer.writeTag(Os, self.*);
    }
};

pub const Abi = enum(u8) {
    gnu,

    pub fn serialize(self: *Abi, writer: *tomboy.ir.binary.Writer) !void {
        try writer.writeTag(Abi, self.*);
    }
};

cpu: Cpu,
os: Os,
abi: Abi,

pub fn serialize(self: *Self, writer: *tomboy.ir.binary.Writer) !void {
    try writer.begin(.object, @typeName(Self));

    try self.cpu.call(.serialize, .{writer});
    try self.os.serialize(writer);
    try self.abi.serialize(writer);
    return try writer.end();
}

pub fn deinit(self: *Self, _: Allocator) void {
    self.cpu.unref();
}
