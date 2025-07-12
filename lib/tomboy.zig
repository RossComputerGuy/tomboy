const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

pub const Target = types.Object(@import("tomboy/Target.zig"));
pub const ir = @import("tomboy/ir.zig");
pub const types = @import("tomboy/types.zig");

test {
    std.testing.refAllDeclsRecursive(@This());
}
