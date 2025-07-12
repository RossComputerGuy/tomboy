const std = @import("std");
const assert = std.debug.assert;
const Self = @This();

pub const Register = enum(u8) {
    // zig fmt: off
    x0, x1, x2, x3, x4, x5, x6, x7, x8,
    x9, x10, x11, x12, x13, x14, x15, x16,
    x17, x18, x19, x20, x21, x22, x23, x24,
    x25, x26, x27, x28, x29, x30, xzr,

    w0, w1, w2, w3, w4, w5, w6, w7, w8, w9,
    w10, w11, w12, w13, w14, w15, w16, w17,
    w18, w19, w20, w21, w22, w23, w24, w25,
    w26, w27, w28, w29, w30, wzr,

    sp, wsp,

    q0, q1, q2, q3, q4, q5, q6, q7, q8, q9,
    q10, q11, q12, q13, q14, q15, q16, q17,
    q18, q19, q20, q21, q22, q23, q24, q25,
    q26, q27, q28, q29, q30, q31,

    d0, d1, d2, d3, d4, d5, d6, d7, d8, d9,
    d10, d11, d12, d13, d14, d15, d16, d17,
    d18, d19, d20, d21, d22, d23, d24, d25,
    d26, d27, d28, d29, d30, d31,

    s0, s1, s2, s3, s4, s5, s6, s7, s8, s9,
    s10, s11, s12, s13, s14, s15, s16, s17,
    s18, s19, s20, s21, s22, s23, s24, s25,
    s26, s27, s28, s29, s30, s31,

    h0, h1, h2, h3, h4, h5, h6, h7, h8, h9,
    h10, h11, h12, h13, h14, h15, h16, h17,
    h18, h19, h20, h21, h22, h23, h24, h25,
    h26, h27, h28, h29, h30, h31,

    b0, b1, b2, b3, b4, b5, b6, b7, b8, b9,
    b10, b11, b12, b13, b14, b15, b16, b17,
    b18, b19, b20, b21, b22, b23, b24, b25,
    b26, b27, b28, b29, b30, b31,
    // zig fmt: on

    const Base = @import("../Register.zig");

    pub const Class = enum {
        gp,
        sp,
        fp,
    };

    pub fn class(self: Register) Class {
        return switch (@intFromEnum(self)) {
            @intFromEnum(Register.x0)...@intFromEnum(Register.xzr) => .gp,
            @intFromEnum(Register.w0)...@intFromEnum(Register.wzr) => .gp,
            @intFromEnum(Register.sp), @intFromEnum(Register.wsp) => .sp,
            @intFromEnum(Register.q0)...@intFromEnum(Register.q31) => .fp,
            @intFromEnum(Register.d0)...@intFromEnum(Register.d31) => .fp,
            @intFromEnum(Register.s0)...@intFromEnum(Register.s31) => .fp,
            @intFromEnum(Register.h0)...@intFromEnum(Register.h31) => .fp,
            @intFromEnum(Register.b0)...@intFromEnum(Register.b31) => .fp,
            else => unreachable,
        };
    }

    pub fn size(self: Register) u8 {
        return switch (@intFromEnum(self)) {
            @intFromEnum(Register.x0)...@intFromEnum(Register.xzr) => 64,
            @intFromEnum(Register.w0)...@intFromEnum(Register.wzr) => 32,
            @intFromEnum(Register.sp) => 64,
            @intFromEnum(Register.wsp) => 32,
            @intFromEnum(Register.q0)...@intFromEnum(Register.q31) => 128,
            @intFromEnum(Register.d0)...@intFromEnum(Register.d31) => 64,
            @intFromEnum(Register.s0)...@intFromEnum(Register.s31) => 32,
            @intFromEnum(Register.h0)...@intFromEnum(Register.h31) => 16,
            @intFromEnum(Register.b0)...@intFromEnum(Register.b31) => 8,
            else => unreachable,
        };
    }

    pub fn id(self: Register) u8 {
        return switch (@intFromEnum(self)) {
            @intFromEnum(Register.x0)...@intFromEnum(Register.xzr) => @as(u8, @intCast(@intFromEnum(self) - @intFromEnum(Register.x0))),
            @intFromEnum(Register.w0)...@intFromEnum(Register.wzr) => @as(u8, @intCast(@intFromEnum(self) - @intFromEnum(Register.w0))),
            @intFromEnum(Register.sp), @intFromEnum(Register.wsp) => 32,
            @intFromEnum(Register.q0)...@intFromEnum(Register.q31) => @as(u8, @intCast(@intFromEnum(self) - @intFromEnum(Register.q0) + 33)),
            @intFromEnum(Register.d0)...@intFromEnum(Register.d31) => @as(u8, @intCast(@intFromEnum(self) - @intFromEnum(Register.d0) + 33)),
            @intFromEnum(Register.s0)...@intFromEnum(Register.s31) => @as(u8, @intCast(@intFromEnum(self) - @intFromEnum(Register.s0) + 33)),
            @intFromEnum(Register.h0)...@intFromEnum(Register.h31) => @as(u8, @intCast(@intFromEnum(self) - @intFromEnum(Register.h0) + 33)),
            @intFromEnum(Register.b0)...@intFromEnum(Register.b31) => @as(u8, @intCast(@intFromEnum(self) - @intFromEnum(Register.b0) + 33)),
            else => unreachable,
        };
    }

    pub fn decode(ec: u5, sf: u2, is_stack_pointer: bool) ?Register {
        if (ec == 31) {
            return switch (sf) {
                0b1 => if (is_stack_pointer) .sp else .xzr,
                0b0 => if (is_stack_pointer) .wsp else .wzr,
                else => null,
            };
        }

        const offset = switch (sf) {
            0b0 => @intFromEnum(Register.x0),
            0b1 => @intFromEnum(Register.w0),
            else => return null,
        };

        return @enumFromInt(offset + ec);
    }

    pub fn encode(self: Register) u5 {
        return switch (@intFromEnum(self)) {
            @intFromEnum(Register.x0)...@intFromEnum(Register.xzr) => @as(u5, @intCast(@intFromEnum(self) - @intFromEnum(Register.x0))),
            @intFromEnum(Register.w0)...@intFromEnum(Register.wzr) => @as(u5, @intCast(@intFromEnum(self) - @intFromEnum(Register.w0))),
            @intFromEnum(Register.sp), @intFromEnum(Register.wsp) => 31,
            @intFromEnum(Register.q0)...@intFromEnum(Register.q31) => @as(u5, @intCast(@intFromEnum(self) - @intFromEnum(Register.q0))),
            @intFromEnum(Register.d0)...@intFromEnum(Register.d31) => @as(u5, @intCast(@intFromEnum(self) - @intFromEnum(Register.d0))),
            @intFromEnum(Register.s0)...@intFromEnum(Register.s31) => @as(u5, @intCast(@intFromEnum(self) - @intFromEnum(Register.s0))),
            @intFromEnum(Register.h0)...@intFromEnum(Register.h31) => @as(u5, @intCast(@intFromEnum(self) - @intFromEnum(Register.h0))),
            @intFromEnum(Register.b0)...@intFromEnum(Register.b31) => @as(u5, @intCast(@intFromEnum(self) - @intFromEnum(Register.b0))),
            else => unreachable,
        };
    }

    pub fn get(self: Register) Base {
        return .{
            .id = self.id(),
            .name = @tagName(self),
            .class = @tagName(self.class()),
            .size = self.size(),
        };
    }
};

pub const LoadStore = @import("AArch64/LoadStore.zig");

pub const Instruction = union(enum) {
    movw_imm: MoveWideImmediate,
    pc_rel_addr: PcRelativeAddress,
    load_store_reg: LoadStore.Register,

    const Base = @import("../Instruction.zig");

    pub fn decode(endian: std.builtin.Endian, reader: anytype) !?Instruction {
        const op = try reader.readInt(u32, endian);
        inline for (std.meta.fields(Instruction)) |base_field| {
            inline for (std.meta.fields(base_field.type)) |field| {
                if (std.mem.eql(u8, field.name, "fixed")) {
                    const fixed = field.defaultValue() orelse unreachable;
                    const instr: base_field.type = @bitCast(op);
                    if (instr.fixed == fixed) {
                        return @unionInit(Instruction, base_field.name, instr);
                    }
                }
            }
        }
        return null;
    }

    pub fn encode(self: Instruction, endian: std.builtin.Endian, writer: anytype) !void {
        return switch (self) {
            inline else => |v| writer.writeInt(u32, @bitCast(v), endian),
        };
    }

    pub fn get(self: Instruction) Base {
        return switch (self) {
            inline else => |v| v.get(),
        };
    }

    pub fn init(base: Base) ?Instruction {
        const mnemonic = std.meta.stringToEnum(Mnemonic, base.name) orelse return null;
        return switch (mnemonic) {
            .movn => .movn(
                std.meta.stringToEnum(Register, base.operands[0].reg) orelse return null,
                base.operands[1].imm_u16,
                @intCast(base.operands[2].shift.amount),
            ),
            .movz => .movz(
                std.meta.stringToEnum(Register, base.operands[0].reg) orelse return null,
                base.operands[1].imm_u16,
                @intCast(base.operands[2].shift.amount),
            ),
            .movk => .movk(
                std.meta.stringToEnum(Register, base.operands[0].reg) orelse return null,
                base.operands[1].imm_u16,
                @intCast(base.operands[2].shift.amount),
            ),
            .adr => .adr(
                std.meta.stringToEnum(Register, base.operands[0].reg) orelse return null,
                base.operands[1].imm_i21,
            ),
            .adrp => .adrp(
                std.meta.stringToEnum(Register, base.operands[0].reg) orelse return null,
                base.operands[1].imm_i21,
            ),
            .ldr => .ldr(
                std.meta.stringToEnum(Register, base.operands[0].reg) orelse return null,
                std.meta.stringToEnum(Register, base.operands[1].reg) orelse return null,
                LoadStore.Offset.init(base.operands[2].mem) catch return null,
            ),
            .ldrh => .ldrh(
                std.meta.stringToEnum(Register, base.operands[0].reg) orelse return null,
                std.meta.stringToEnum(Register, base.operands[1].reg) orelse return null,
                LoadStore.Offset.init(base.operands[2].mem) catch return null,
            ),
            .ldrb => .ldrb(
                std.meta.stringToEnum(Register, base.operands[0].reg) orelse return null,
                std.meta.stringToEnum(Register, base.operands[1].reg) orelse return null,
                LoadStore.Offset.init(base.operands[2].mem) catch return null,
            ),
            .ldrsb => .ldrsb(
                std.meta.stringToEnum(Register, base.operands[0].reg) orelse return null,
                std.meta.stringToEnum(Register, base.operands[1].reg) orelse return null,
                LoadStore.Offset.init(base.operands[2].mem) catch return null,
            ),
            .ldrsh => .ldrsh(
                std.meta.stringToEnum(Register, base.operands[0].reg) orelse return null,
                std.meta.stringToEnum(Register, base.operands[1].reg) orelse return null,
                LoadStore.Offset.init(base.operands[2].mem) catch return null,
            ),
            .ldrsw => .ldrsw(
                std.meta.stringToEnum(Register, base.operands[0].reg) orelse return null,
                std.meta.stringToEnum(Register, base.operands[1].reg) orelse return null,
                LoadStore.Offset.init(base.operands[2].mem) catch return null,
            ),
            .str => .str(
                std.meta.stringToEnum(Register, base.operands[0].reg) orelse return null,
                std.meta.stringToEnum(Register, base.operands[1].reg) orelse return null,
                LoadStore.Offset.init(base.operands[2].mem) catch return null,
            ),
            .strh => .strh(
                std.meta.stringToEnum(Register, base.operands[0].reg) orelse return null,
                std.meta.stringToEnum(Register, base.operands[1].reg) orelse return null,
                LoadStore.Offset.init(base.operands[2].mem) catch return null,
            ),
            .strb => .strb(
                std.meta.stringToEnum(Register, base.operands[0].reg) orelse return null,
                std.meta.stringToEnum(Register, base.operands[1].reg) orelse return null,
                LoadStore.Offset.init(base.operands[2].mem) catch return null,
            ),
        };
    }

    pub fn movn(rd: Register, imm16: u16, shift: u6) Instruction {
        return .{ .movw_imm = .init(0b00, rd, imm16, shift) };
    }

    pub fn movz(rd: Register, imm16: u16, shift: u6) Instruction {
        return .{ .movw_imm = .init(0b10, rd, imm16, shift) };
    }

    pub fn movk(rd: Register, imm16: u16, shift: u6) Instruction {
        return .{ .movw_imm = .init(0b11, rd, imm16, shift) };
    }

    pub fn adr(rd: Register, imm21: i21) Instruction {
        return .{ .pc_rel_addr = .init(rd, imm21, 0b0) };
    }

    pub fn adrp(rd: Register, imm21: i21) Instruction {
        return .{ .pc_rel_addr = .init(rd, imm21, 0b1) };
    }

    pub fn ldr(rt: Register, rn: Register, offset: LoadStore.Offset) Instruction {
        return .{ .load_store_reg = .init(rt, rn, offset, .ldr) };
    }

    pub fn ldrh(rt: Register, rn: Register, offset: LoadStore.Offset) Instruction {
        return .{ .load_store_reg = .init(rt, rn, offset, .ldrh) };
    }

    pub fn ldrb(rt: Register, rn: Register, offset: LoadStore.Offset) Instruction {
        return .{ .load_store_reg = .init(rt, rn, offset, .ldrb) };
    }

    pub fn ldrsb(rt: Register, rn: Register, offset: LoadStore.Offset) Instruction {
        return .{ .load_store_reg = .init(rt, rn, offset, .ldrsb) };
    }

    pub fn ldrsh(rt: Register, rn: Register, offset: LoadStore.Offset) Instruction {
        return .{ .load_store_reg = .init(rt, rn, offset, .ldrsh) };
    }

    pub fn ldrsw(rt: Register, rn: Register, offset: LoadStore.Offset) Instruction {
        return .{ .load_store_reg = .init(rt, rn, offset, .ldrsw) };
    }

    pub fn str(rt: Register, rn: Register, offset: LoadStore.Offset) Instruction {
        return .{ .load_store_reg = .init(rt, rn, offset, .str) };
    }

    pub fn strh(rt: Register, rn: Register, offset: LoadStore.Offset) Instruction {
        return .{ .load_store_reg = .init(rt, rn, offset, .strh) };
    }

    pub fn strb(rt: Register, rn: Register, offset: LoadStore.Offset) Instruction {
        return .{ .load_store_reg = .init(rt, rn, offset, .strb) };
    }

    pub const Mnemonic = enum {
        movn,
        movz,
        movk,
        adr,
        adrp,
        ldr,
        ldrh,
        ldrb,
        ldrsb,
        ldrsh,
        ldrsw,
        str,
        strh,
        strb,

        pub fn info(self: Mnemonic) Base.Info {
            return .{
                .name = @tagName(self),
                .operands = switch (self) {
                    .movn, .movz, .movk => &.{
                        .reg,
                        .imm_u16,
                        .shift,
                    },
                    .adr, .adrp => &.{
                        .reg,
                        .imm_i21,
                    },
                    .ldr,
                    .ldrh,
                    .ldrb,
                    .ldrsb,
                    .ldrsh,
                    .ldrsw,
                    .str,
                    .strh,
                    .strb,
                    => &.{
                        .reg,
                        .reg,
                        .mem,
                    },
                },
                .size = @bitSizeOf(u32),
            };
        }
    };

    pub const MoveWideImmediate = packed struct {
        rd: u5,
        imm16: u16,
        hw: u2,
        fixed: u6 = 0b100101,
        opc: u2,
        sf: u1,

        pub fn init(opc: u2, rd: Register, imm16: u16, shift: u6) MoveWideImmediate {
            assert(shift % 16 == 0);
            assert(!(rd.size() == 32 and shift > 16));
            assert(!(rd.size() == 64 and shift > 48));
            return .{
                .rd = rd.encode(),
                .imm16 = imm16,
                .hw = @as(u2, @intCast(shift / 16)),
                .opc = opc,
                .sf = switch (rd.size()) {
                    32 => 0,
                    64 => 1,
                    else => unreachable,
                },
            };
        }

        pub fn get(self: MoveWideImmediate) Base {
            return .{
                .name = switch (self.opc) {
                    0b00 => "movn",
                    0b10 => "movz",
                    0b11 => "movk",
                    else => unreachable,
                },
                .operands = &.{
                    .{ .reg = @tagName(Register.decode(self.rd, self.sf, false) orelse unreachable) },
                    .{ .imm_u16 = self.imm16 },
                    .{ .shift = .{
                        .kind = .lsl,
                        .amount = @as(u6, @intCast(self.hw)) * 16,
                    } },
                },
                .size = @bitSizeOf(u32),
            };
        }
    };

    pub const PcRelativeAddress = packed struct {
        rd: u5,
        immhi: u19,
        fixed: u5 = 0b10000,
        immlo: u2,
        op: u1,

        pub fn init(rd: Register, imm21: i21, op: u1) PcRelativeAddress {
            assert(rd.size() == 64);
            const imm21_u = @as(u21, @bitCast(imm21));
            return .{
                .rd = rd.encode(),
                .immlo = @as(u2, @truncate(imm21_u)),
                .immhi = @as(u19, @truncate(imm21_u >> 2)),
                .op = op,
            };
        }

        pub fn get(self: PcRelativeAddress) Base {
            return .{
                .name = switch (self.op) {
                    0b0 => "adr",
                    0b1 => "adrp",
                },
                .operands = &.{
                    .{ .reg = @tagName(Register.decode(self.rd, 1, false) orelse unreachable) },
                    .{ .imm_i21 = (@as(i21, self.immhi) << 2) | @as(i21, self.immlo) },
                },
                .size = @bitSizeOf(u32),
            };
        }
    };
};
