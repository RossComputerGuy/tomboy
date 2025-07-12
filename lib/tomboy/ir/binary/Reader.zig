const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const binary = @import("../binary.zig");
const Self = @This();

pub const ReadSection = struct {
    start: binary.Section,
    payload: std.io.FixedBufferStream([]u8),

    pub fn deinit(self: *ReadSection, alloc: Allocator) void {
        self.start.deinit(alloc);
        alloc.free(self.payload.buffer);
    }

    pub fn reader(self: *ReadSection) std.io.FixedBufferStream([]u8).Reader {
        return self.payload.reader();
    }
};

byte_reader: std.io.AnyReader,
endian: std.builtin.Endian,
mutex: std.Thread.Mutex,
read_sections: std.ArrayList(ReadSection),

pub fn init(alloc: Allocator, reader: std.io.AnyReader, endian: ?std.builtin.Endian) Self {
    return .{
        .byte_reader = reader,
        .endian = endian orelse builtin.cpu.arch.endian(),
        .mutex = .{},
        .read_sections = .init(alloc),
    };
}

pub fn deinit(self: *Self) void {
    for (self.read_sections.items) |*read_section| {
        read_section.deinit(self.read_sections.allocator);
    }
    self.read_sections.deinit();
}

pub fn section(self: *Self) !*ReadSection {
    const len = self.read_sections.items.len;
    if (len == 0) return error.NoSections;
    return &self.read_sections.items[len - 1];
}

pub fn sectionWithRead(self: *Self) !*ReadSection {
    return self.section() catch |err| switch (err) {
        error.NoSections => self.readSection(),
    };
}

pub fn findSection(self: *Self, tag: binary.Tag, name: []const u8) !*ReadSection {
    while (true) {
        const read_section = try self.sectionWithRead();
        if (read_section.start.tag != tag) continue;
        if (!std.mem.eql(u8, read_section.start.name(), name)) continue;
        return read_section;
    }
}

pub fn readString(self: *Self) ![]const u8 {
    const read_section = try self.section();

    const alloc = self.read_sections.allocator;

    const len = try read_section.reader().readInt(u64, self.endian);

    const string = try alloc.alloc(u8, len);
    errdefer alloc.free(string);

    for (string) |*byte| {
        const x = try read_section.reader().readByte();
        byte.* = std.mem.nativeTo(u8, x, self.endian);
    }

    return string;
}

pub fn readInt(self: *Self, comptime T: type) !T {
    const read_section = try self.section();
    return try read_section.reader().readInt(u64, self.endian);
}

pub fn readSection(self: *Self) !*ReadSection {
    const alloc = self.read_sections.allocator;

    self.mutex.lock();
    defer self.mutex.unlock();

    const read_section = try self.read_sections.addOne();
    errdefer _ = self.read_sections.pop();

    read_section.start = try .read(alloc, self.byte_reader, self.endian);
    errdefer read_section.start.deinit(alloc);

    read_section.payload = .{
        .buffer = try alloc.alloc(u8, read_section.start.size),
        .pos = 0,
    };
    errdefer alloc.free(read_section.payload.buffer);

    _ = try self.byte_reader.readAll(read_section.payload.buffer);

    return read_section;
}

test {
    const alloc = std.testing.allocator;

    var fbs = std.io.fixedBufferStream(@as([]const u8, &.{
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
    }));

    var reader = init(alloc, fbs.reader().any(), null);
    defer reader.deinit();

    _ = try reader.readSection();

    const str = try reader.readString();
    defer alloc.free(str);

    try std.testing.expectEqualStrings("A", str);
}
