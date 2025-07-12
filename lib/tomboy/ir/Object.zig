const std = @import("std");
const Allocator = std.mem.Allocator;
const Source = @import("Source.zig");
const Self = @This();

pub const Type = union(enum) {
    module: Module,

    pub fn scope(self: *Type) ?*Scope {
        return switch (self.*) {
            inline else => |*v| blk: {
                const T = @TypeOf(v.*);
                if (@hasDecl(T, "getScope")) break :blk v.getScope();
                if (@hasField(T, "scope")) break :blk &v.scope;
                break :blk null;
            },
        };
    }

    pub fn deinit(self: *Type, alloc: Allocator) void {
        return switch (self.*) {
            inline else => |*v| if (@hasDecl(@TypeOf(v.*), "deinit")) v.deinit(alloc),
        };
    }

    pub const Module = @import("Object/Type/Module.zig");
    pub const Scope = @import("Object/Type/Scope.zig");
};

source: ?Source,
type: Type,

pub fn deinit(self: *Self, alloc: Allocator) void {
    if (self.source) |*src| src.deinit(alloc);
    self.type.deinit(alloc);
}
