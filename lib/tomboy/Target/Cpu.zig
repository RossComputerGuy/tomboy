const std = @import("std");
const Allocator = std.mem.Allocator;
const tomboy = @import("../../tomboy.zig");
const Self = @This();

pub const Instruction = @import("Cpu/Instruction.zig");
pub const Register = @import("Cpu/Register.zig");

pub const Arch = enum(u8) {
    aarch64,

    pub const AArch64 = @import("Cpu/Arch/AArch64.zig");

    pub fn getType(comptime self: Arch) type {
        return switch (self) {
            .aarch64 => AArch64,
        };
    }

    pub fn listRegisters(self: Arch, alloc: Allocator) ![]const []const u8 {
        inline for (std.meta.fields(Arch)) |field| {
            const t: Arch = @enumFromInt(field.value);
            if (t == self) {
                const arch = getType(t);
                const tags = std.meta.tags(arch.Register);

                const list = try alloc.alloc([]const u8, tags.len);
                errdefer alloc.free(list);

                for (list, tags) |*l, tag| {
                    l.* = @tagName(tag);
                }

                return list;
            }
        }
        unreachable;
    }

    pub fn listInstructions(self: Arch, alloc: Allocator) ![]const []const u8 {
        inline for (std.meta.fields(Arch)) |field| {
            const t: Arch = @enumFromInt(field.value);
            if (t == self) {
                const arch = getType(t);
                const tags = std.meta.tags(arch.Instruction.Mnemonic);

                const list = try alloc.alloc([]const u8, tags.len);
                errdefer alloc.free(list);

                for (list, tags) |*l, tag| {
                    l.* = @tagName(tag);
                }

                return list;
            }
        }
        unreachable;
    }

    pub fn getInstructionInfo(self: Arch, name: []const u8) ?Instruction.Info {
        inline for (std.meta.fields(Arch)) |field| {
            const t: Arch = @enumFromInt(field.value);
            if (t == self) {
                const arch = getType(t);
                const mnemonic = std.meta.stringToEnum(arch.Instruction.Mnemonic, name) orelse return null;
                return mnemonic.info();
            }
        }
        return null;
    }

    pub fn decodeInstruction(self: Arch, endian: std.builtin.Endian, reader: anytype) !?Instruction {
        inline for (std.meta.fields(Arch)) |field| {
            const t: Arch = @enumFromInt(field.value);
            if (t == self) {
                const arch = getType(t);
                if (arch.Instruction.decode(endian, reader)) |instr| {
                    return instr.get();
                }
            }
        }
        return null;
    }

    pub fn encodeInstruction(self: Arch, base: Instruction, endian: std.builtin.Endian, writer: anytype) !void {
        inline for (std.meta.fields(Arch)) |field| {
            const t: Arch = @enumFromInt(field.value);
            if (t == self) {
                const arch = getType(t);
                if (arch.Instruction.init(base)) |instr| {
                    return try instr.encode(endian, writer);
                }
                return error.InvalidInstruction;
            }
        }
        unreachable;
    }

    pub fn getRegister(self: Arch, name: []const u8) ?Register {
        inline for (std.meta.fields(Arch)) |field| {
            const t: Arch = @enumFromInt(field.value);
            if (t == self) {
                const arch = getType(t);
                const reg = std.meta.stringToEnum(arch.Register, name) orelse return null;
                return reg.get();
            }
        }
        return null;
    }

    pub fn serialize(self: Arch, writer: *tomboy.ir.binary.Writer) !void {
        try writer.writeTag(Arch, self);
    }
};

arch: Arch,

pub fn serialize(self: *Self, writer: *tomboy.ir.binary.Writer) !void {
    try writer.begin(.object, @typeName(Self));

    try self.arch.serialize(writer);
    return try writer.end();
}

test {
    inline for (std.meta.tags(Arch)) |arch| {
        const regs = try arch.listRegisters(std.testing.allocator);
        defer std.testing.allocator.free(regs);

        for (regs) |reg_name| {
            std.debug.print("{?}\n", .{arch.getRegister(reg_name)});
        }

        const instrs = try arch.listInstructions(std.testing.allocator);
        defer std.testing.allocator.free(instrs);

        for (instrs) |instr_name| {
            std.debug.print("{?}\n", .{arch.getInstructionInfo(instr_name)});
        }
    }
}
