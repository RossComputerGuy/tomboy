const std = @import("std");
const assert = std.debug.assert;
const math = @import("../../../../math.zig");
const InstructionBase = @import("../../Instruction.zig");
const AArch64 = @import("../AArch64.zig");
const Self = @This();

pub const Variant = enum {
    str,
    strb,
    strh,
    ldr,
    ldrb,
    ldrh,
    ldrsb,
    ldrsh,
    ldrsw,
};

pub const Offset = union(enum) {
    imm: Immediate,
    reg: Offset.Register,

    pub fn init(mem: InstructionBase.Operand.Memory) !Offset {
        if (mem.index) |index| {
            const index_reg = std.meta.stringToEnum(AArch64.Register, index) orelse return error.InvalidRegister;

            const shift_amt: u2 = switch (mem.scale) {
                1 => 0,
                4 => 2,
                8 => 3,
                else => return error.UnsupportedShiftAmount,
            };

            return if (mem.extend) |ext| switch (ext) {
                .uxtw => .initRegUxtw(index_reg, shift_amt),
                .sxtw => .initRegSxtw(index_reg, shift_amt),
                .sxtx => .initRegSxtx(index_reg, shift_amt),
                .uxtx => error.InvalidExtension,
            } else .initRegLsl(index_reg, shift_amt);
        }

        return switch (mem.kind) {
            .pre_indexed => blk: {
                if (mem.offset < -256 or mem.offset > 255) break :blk error.OffsetOutOfRange;
                break :blk .initImmPreIndexed(@intCast(mem.offset));
            },
            .post_indexed => blk: {
                if (mem.offset < -256 or mem.offset > 255) break :blk error.OffsetOutOfRange;
                break :blk .initImmPostIndexed(@intCast(mem.offset));
            },
            .direct => blk: {
                if (mem.offset < 0 or mem.offset > 4095) break :blk error.OffsetOutOfRange;
                break :blk .initImm(@intCast(mem.offset));
            },
            .pc_rel => error.InvalidKind,
        };
    }

    pub fn initRegUxtw(rm: AArch64.Register, shift: u2) Offset {
        assert(rm.size() == 32 and (shift == 0 or shift == 2));
        return .{ .reg = .{
            .rm = rm,
            .shift = .{ .uxtw = shift },
        } };
    }

    pub fn initRegLsl(rm: AArch64.Register, shift: u2) Offset {
        assert(rm.size() == 64 and (shift == 0 or shift == 3));
        return .{ .reg = .{
            .rm = rm,
            .shift = .{ .lsl = shift },
        } };
    }

    pub fn initRegSxtw(rm: AArch64.Register, shift: u2) Offset {
        assert(rm.size() == 32 and (shift == 0 or shift == 2));
        return .{ .reg = .{
            .rm = rm,
            .shift = .{ .sxtw = shift },
        } };
    }

    pub fn initRegSxtx(rm: AArch64.Register, shift: u2) Offset {
        assert(rm.size() == 64 and (shift == 0 or shift == 3));
        return .{ .reg = .{
            .rm = rm,
            .shift = .{ .sxtx = shift },
        } };
    }

    pub fn initImmPreIndexed(offset: i9) Offset {
        return .{ .imm = .{ .pre_index = offset } };
    }

    pub fn initImmPostIndexed(offset: i9) Offset {
        return .{ .imm = .{ .post_index = offset } };
    }

    pub fn initImm(offset: u12) Offset {
        return .{ .imm = .{ .unsigned = offset } };
    }

    pub const none = Offset{
        .imm = .{ .unsigned = 0 },
    };

    pub fn decode(op1: u2, offset: u12) ?Offset {
        if (op1 == 0b01) {
            return .initImm(offset);
        } else if (op1 == 0b00) {
            const mode = offset & 0b11;
            return switch (mode) {
                1 => blk: {
                    const imm9_raw: u9 = @intCast(offset >> 2);
                    const imm9: i9 = @intCast(math.signExtend(u9, imm9_raw, 9));
                    break :blk .initImmPostIndexed(imm9);
                },
                2 => blk: {
                    const imm9_raw: u9 = @intCast(offset >> 2);
                    const imm9: i9 = @intCast(math.signExtend(u9, imm9_raw, 9));
                    break :blk .initImmPreIndexed(imm9);
                },
                else => blk: {
                    const rm_enc: u5 = @intCast((offset >> 6) & 0x1F);
                    const shift_val: u2 = @intCast((offset >> 2) & 0x3);

                    const rm = AArch64.Register.decode(rm_enc, 1, false) orelse break :blk null;

                    break :blk .{ .reg = .{
                        .rm = rm,
                        .shift = switch (rm.size()) {
                            64 => if (shift_val == 0 or shift_val == 3) .{ .lsl = shift_val } else break :blk null,
                            32 => if (shift_val == 0 or shift_val == 2) .{ .uxtw = shift_val } else break :blk null,
                            else => unreachable,
                        },
                    } };
                },
            };
        }
        return null;
    }

    pub fn encode(self: Offset) u12 {
        return switch (self) {
            .imm => |imm_type| switch (imm_type) {
                .post_index => |v| (@as(u12, @intCast(@as(u9, @bitCast(v)))) << 2) + 1,
                .pre_index => |v| (@as(u12, @intCast(@as(u9, @bitCast(v)))) << 2) + 3,
                .unsigned => |v| v,
            },
            .reg => |r| switch (r.shift) {
                .uxtw => |v| (@as(u12, @intCast(r.rm.encode())) << 6) + (@as(u12, @intCast(v)) << 2) + 16 + 2050,
                .lsl => |v| (@as(u12, @intCast(r.rm.encode())) << 6) + (@as(u12, @intCast(v)) << 2) + 24 + 2050,
                .sxtw => |v| (@as(u12, @intCast(r.rm.encode())) << 6) + (@as(u12, @intCast(v)) << 2) + 48 + 2050,
                .sxtx => |v| (@as(u12, @intCast(r.rm.encode())) << 6) + (@as(u12, @intCast(v)) << 2) + 56 + 2050,
            },
        };
    }

    pub fn toMemoryOperand(self: Offset, base: AArch64.Register) InstructionBase.Operand.Memory {
        return switch (self) {
            .imm => |imm| switch (imm) {
                .post_index => |v| .{
                    .base = @tagName(base),
                    .offset = @intCast(v),
                    .kind = .post_indexed,
                },
                .pre_index => |v| .{
                    .base = @tagName(base),
                    .offset = @intCast(v),
                    .kind = .pre_indexed,
                },
                .unsigned => |v| .{
                    .base = @tagName(base),
                    .offset = @intCast(v),
                    .kind = .direct,
                },
            },
            .reg => |reg| .{
                .base = @tagName(base),
                .index = @tagName(reg.rm),
                .scale = switch (reg.shift) {
                    inline else => |i| (@as(u8, 1)) << i,
                },
                .kind = .direct,
                .extend = switch (reg.shift) {
                    .lsl => null,
                    .uxtw => .uxtw,
                    .sxtw => .sxtw,
                    .sxtx => .sxtx,
                },
            },
        };
    }

    pub const Immediate = union(enum) {
        post_index: i9,
        pre_index: i9,
        unsigned: u12,
    };

    pub const Register = struct {
        rm: AArch64.Register,
        shift: Shift,

        pub const Shift = union(enum) {
            uxtw: u2,
            lsl: u2,
            sxtw: u2,
            sxtx: u2,
        };
    };
};

pub const Register = packed struct {
    rt: u5,
    rn: u5,
    offset: u12,
    opc: u2,
    op1: u2,
    v: u1,
    fixed: u3 = 0b111,
    size: u2,

    pub fn init(rt: AArch64.Register, rn: AArch64.Register, offset: Offset, variant: Variant) Register {
        assert(rn.size() == 64);
        assert(rn.id() != AArch64.Register.xzr.id());

        const op1: u2 = blk: {
            switch (offset) {
                .imm => |imm| switch (imm) {
                    .unsigned => break :blk 0b01,
                    else => {},
                },
                else => {},
            }
            break :blk 0b00;
        };

        const opc: u2 = switch (variant) {
            .ldr, .ldrh, .ldrb => 0b01,
            .str, .strh, .strb => 0b00,
            .ldrsb,
            .ldrsh,
            => switch (rt.size()) {
                32 => 0b11,
                64 => 0b10,
                else => unreachable,
            },
            .ldrsw => 0b10,
        };

        const size: u2 = switch (variant) {
            .ldr, .str => switch (rt.size()) {
                32 => 0b10,
                64 => 0b11,
                else => unreachable,
            },
            .ldrsw => 0b10,
            .ldrh, .ldrsh, .strh => 0b01,
            .ldrb, .ldrsb, .strb => 0b00,
        };

        return .{
            .rt = rt.encode(),
            .rn = rn.encode(),
            .offset = offset.encode(),
            .opc = opc,
            .op1 = op1,
            .v = 0,
            .size = size,
        };
    }

    pub fn get(self: Register) InstructionBase {
        const base = AArch64.Register.decode(self.rn, 1, false) orelse unreachable;
        const offset = Offset.decode(self.op1, self.offset) orelse unreachable;
        return .{
            .name = switch (self.opc) {
                0b01 => switch (self.size) {
                    0b10, 0b11 => "ldr",
                    0b01 => "ldrh",
                    0b00 => "ldrb",
                },
                0b00 => switch (self.size) {
                    0b10, 0b11 => "str",
                    0b01 => "strh",
                    0b00 => "strbb",
                },
                0b11 => switch (self.size) {
                    0b01 => "ldrsh",
                    0b00 => "ldrsb",
                    else => unreachable,
                },
                0b10 => switch (self.size) {
                    0b10 => "ldrsw",
                    else => unreachable,
                },
            },
            .operands = &.{
                .{ .reg = @tagName(base) },
                .{ .reg = @tagName(AArch64.Register.decode(self.rt, 1, false) orelse unreachable) },
                .{ .mem = offset.toMemoryOperand(base) },
            },
            .size = @bitSizeOf(u32),
        };
    }
};
