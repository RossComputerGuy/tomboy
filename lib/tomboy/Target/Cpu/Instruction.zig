const std = @import("std");

pub const Operand = union(enum) {
    reg: []const u8,
    imm_u16: u16,
    imm_i16: i16,
    imm_u21: u21,
    imm_i21: i21,
    imm_u32: u32,
    imm_i32: i32,
    imm_u64: u64,
    imm_i64: i64,
    mem: Memory,
    cond: Condition,
    shift: Shift,
    extend: Extend,
    vector: Vector,
    sys: []const u8,

    pub const Memory = struct {
        base: ?[]const u8,
        index: ?[]const u8 = null,
        scale: u8 = 1,
        offset: i64 = 0,
        kind: Kind,
        extend: ?Extend.Kind = null,

        pub const Kind = enum {
            direct,
            pre_indexed,
            post_indexed,
            pc_rel,
        };
    };

    pub const Condition = enum {
        eq,
        ne,
        lt,
        le,
        gt,
        ge,
        cs,
        cc,
        always,
    };

    pub const Shift = struct {
        kind: Kind,
        amount: u8,

        pub const Kind = enum {
            lsl,
            lsr,
            asr,
            ror,
        };
    };

    pub const Extend = struct {
        kind: Kind,
        amount: u8,

        pub const Kind = enum {
            uxtw,
            sxtw,
            uxtx,
            sxtx,
        };
    };

    pub const Vector = struct {
        id: []const u8,
        size: u8,
        element_type: ElementType,
        element_count: ?u8 = null,

        pub const ElementType = enum {
            b,
            h,
            s,
            d,
            q,
        };
    };
};

pub const Info = struct {
    name: []const u8,
    operands: []const std.meta.Tag(Operand),
    size: u8,
};

name: []const u8,
operands: []const Operand,
size: u8,
