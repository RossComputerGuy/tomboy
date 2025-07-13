const std = @import("std");
const assert = std.debug.assert;
const math = @import("../../../../../math.zig");
const InstructionBase = @import("../../../Instruction.zig");
const AArch64 = @import("../../AArch64.zig");
const Self = @This();

pub const Offset = struct {
    encoding: Encoding,
    offset: i9,

    pub const Encoding = enum(u2) {
        post_index = 0b01,
        signed = 0b10,
        pre_index = 0b11,
    };

    pub fn init(mem: InstructionBase.Operand.Memory) !Offset {
        return .{
            .encoding = switch (mem.kind) {
                .direct => .signed,
                .pre_indexed => .pre_index,
                .post_indexed => .post_index,
                else => return error.InvalidEncoding,
            },
            .offset = @intCast(mem.offset),
        };
    }

    pub fn toMemoryOperand(self: Offset, rt: AArch64.Register) InstructionBase.Operand.Memory {
        return .{
            .base = @tagName(rt),
            .offset = self.offset,
            .kind = switch (self.encoding) {
                .post_index => .post_indexed,
                .signed => .direct,
                .pre_index => .pre_indexed,
            },
        };
    }
};

pub const Register = packed struct {
    rt1: u5,
    rn: u5,
    rt2: u5,
    imm7: u7,
    load: u1,
    encoding: u2,
    fixed: u5 = 0b101_0_0,
    opc: u2,

    pub fn init(rt1: AArch64.Register, rt2: AArch64.Register, rn: AArch64.Register, offset: i9, encoding: u2, load: bool) Register {
        assert(rn.size() == 64);
        assert(rn.id() != AArch64.Register.xzr.id());

        const imm7: u7, const opc: u2 = switch (rt1.size()) {
            32 => blk: {
                assert(-256 <= offset and offset <= 252);
                break :blk .{ @as(u7, @truncate(@as(u9, @bitCast(offset >> 2)))), 0b00 };
            },
            64 => blk: {
                assert(-512 <= offset and offset <= 504);
                break :blk .{ @as(u7, @truncate(@as(u9, @bitCast(offset >> 3)))), 0b10 };
            },
            else => unreachable,
        };

        return .{
            .rt1 = rt1.encode(),
            .rn = rn.encode(),
            .rt2 = rt2.encode(),
            .imm7 = imm7,
            .load = @intFromBool(load),
            .encoding = encoding,
            .opc = opc,
        };
    }

    pub fn get(self: Register) InstructionBase {
        const rt1 = AArch64.Register.decode(self.rt1, self.opc & (1 << 0), false) orelse unreachable;
        const offset = Offset{
            .encoding = @enumFromInt(self.encoding),
            .offset = @intCast(@as(i7, @bitCast(self.imm7))),
        };
        return .{
            .name = switch (self.encoding) {
                0 => if (self.load == 1) "ldnp" else "stnp",
                else => if (self.load == 1) "ldp" else "stp",
            },
            .operands = &.{
                .{ .reg = @tagName(rt1) },
                .{ .reg = @tagName(AArch64.Register.decode(self.rt2, self.opc & (1 << 0), false) orelse unreachable) },
                .{ .reg = @tagName(AArch64.Register.decode(self.rn, 1, false) orelse unreachable) },
                if (self.load == 1) .{
                    .imm_i9 = self.imm7,
                } else .{
                    .mem = offset.toMemoryOperand(rt1),
                },
            },
            .size = @bitSizeOf(u32),
        };
    }
};
