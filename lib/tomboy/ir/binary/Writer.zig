const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const binary = @import("../binary.zig");
const Self = @This();

pub const WriteSection = struct {
    tag: binary.Tag,
    name: []const u8,
    payload: std.ArrayListUnmanaged(u8),

    pub fn section(self: *const WriteSection) binary.Section {
        return .init(self.tag, self.name, self.payload.items.len);
    }

    pub fn deinit(self: *WriteSection, alloc: Allocator) void {
        self.payload.deinit(alloc);
    }

    pub fn writer(self: *WriteSection, alloc: Allocator) std.ArrayListUnmanaged(u8).Writer {
        return self.payload.writer(alloc);
    }
};

byte_writer: std.io.AnyWriter,
mutex: std.Thread.Mutex,
endian: std.builtin.Endian,
write_sections: std.ArrayList(WriteSection),

pub fn init(alloc: Allocator, writer: std.io.AnyWriter, endian: ?std.builtin.Endian) Self {
    return .{
        .byte_writer = writer,
        .mutex = .{},
        .endian = endian orelse builtin.cpu.arch.endian(),
        .write_sections = .init(alloc),
    };
}

pub fn deinit(self: *Self) void {
    assert(self.write_sections.items.len == 0);
    self.write_sections.deinit();
}

fn section(self: *Self) !*WriteSection {
    const len = self.write_sections.items.len;
    if (len == 0) return error.NoSections;
    return &self.write_sections.items[len - 1];
}

pub fn begin(self: *Self, tag: binary.Tag, name: []const u8) !void {
    self.mutex.lock();
    defer self.mutex.unlock();

    const sec = try self.write_sections.addOne();
    errdefer _ = self.sections.pop();

    sec.* = .{
        .tag = tag,
        .name = name,
        .payload = .{},
    };
}

pub fn sectionWriter(self: *Self) !std.ArrayListUnmanaged(u8).Writer {
    const alloc = self.write_sections.allocator;
    const write_section = try self.section();
    return write_section.writer(alloc);
}

pub fn writeString(self: *Self, value: []const u8) !void {
    const section_writer = try self.sectionWriter();

    var buffered_writer = std.io.bufferedWriter(section_writer);
    const writer = buffered_writer.writer();

    try writer.writeInt(u64, value.len, self.endian);

    for (value) |byte| {
        const x = std.mem.nativeTo(u8, byte, self.endian);
        try writer.writeByte(x);
    }

    return try buffered_writer.flush();
}

pub fn writeTag(self: *Self, comptime T: type, value: T) !void {
    try self.writeInt(std.meta.Tag(T), @intFromEnum(value));
}

pub fn writeInt(self: *Self, comptime T: type, value: T) !void {
    const section_writer = try self.sectionWriter();

    var buffered_writer = std.io.bufferedWriter(section_writer);
    const writer = buffered_writer.writer();

    try writer.writeInt(T, value, self.endian);
    return try buffered_writer.flush();
}

pub fn end(self: *Self) !void {
    const alloc = self.write_sections.allocator;
    var write_section = self.write_sections.pop() orelse return error.MissingSection;
    defer write_section.deinit(alloc);

    const sec = write_section.section();

    var buffered_writer = std.io.bufferedWriter(self.byte_writer);
    const writer = buffered_writer.writer();

    try sec.write(writer, self.endian);

    try writer.writeAll(write_section.payload.items);

    return try buffered_writer.flush();
}

test {
    const alloc = std.testing.allocator;

    var output = std.ArrayList(u8).init(alloc);
    defer output.deinit();

    var writer = init(alloc, output.writer().any(), null);
    defer writer.deinit();

    try writer.begin(.object, "Hello, world");
    try writer.writeString("A");
    try writer.end();

    try std.testing.expectEqualSlices(u8, &.{
        // Section tag
        0,
        // Size
        9,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        // Name length
        12,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        // Name
        'H',
        'e',
        'l',
        'l',
        'o',
        ',',
        ' ',
        'w',
        'o',
        'r',
        'l',
        'd',

        // Payload
        1,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        'A',
    }, output.items);
}
