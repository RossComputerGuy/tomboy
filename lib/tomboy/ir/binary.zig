const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Reader = @import("binary/Reader.zig");
pub const Writer = @import("binary/Writer.zig");

pub const Tag = enum(u8) {
    object,
    tagged_union,
};

pub const Section = packed struct {
    tag: Tag,
    size: u64,
    name_len: usize,
    name_buff: [*]const u8,

    pub fn read(alloc: Allocator, reader: anytype, endian: std.builtin.Endian) !Section {
        const tag = try reader.readEnum(Tag, endian);
        const size = try reader.readInt(usize, endian);
        const name_len = try reader.readInt(usize, endian);

        const name_value = try alloc.alloc(u8, name_len);
        errdefer alloc.free(name_value);

        _ = try reader.read(name_value);

        return .{
            .tag = tag,
            .size = size,
            .name_len = name_len,
            .name_buff = name_value.ptr,
        };
    }

    pub fn write(self: *const Section, backing_writer: anytype, endian: std.builtin.Endian) !void {
        var buffered_writer = std.io.bufferedWriter(backing_writer);
        const writer = buffered_writer.writer();

        try writer.writeInt(std.meta.Tag(Tag), @intFromEnum(self.tag), endian);
        try writer.writeInt(u64, self.size, endian);
        try writer.writeInt(u64, self.name_len, endian);

        for (self.name()) |byte| {
            const x = std.mem.nativeTo(u8, byte, endian);
            try writer.writeByte(x);
        }

        return try buffered_writer.flush();
    }

    pub fn init(tag: Tag, name_value: []const u8, size: usize) Section {
        return .{
            .tag = tag,
            .name_len = name_value.len,
            .name_buff = name_value.ptr,
            .size = size,
        };
    }

    pub fn name(self: *const Section) []const u8 {
        return self.name_buff[0..self.name_len];
    }

    pub fn deinit(self: *Section, alloc: Allocator) void {
        alloc.free(self.name());
    }
};
