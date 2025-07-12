const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

pub fn Object(comptime T: type) type {
    return struct {
        const Self = @This();

        pub const Class = T;

        pub const Function = blk: {
            var i: usize = 0;
            for (std.meta.declarations(T)) |d| {
                if (std.meta.hasMethod(T, d.name)) {
                    i += 1;
                }
            }

            var fields: [i]std.builtin.Type.EnumField = undefined;
            i = 0;

            for (std.meta.declarations(T)) |d| {
                if (std.meta.hasMethod(T, d.name)) {
                    fields[i] = .{
                        .name = d.name,
                        .value = i,
                    };
                    i += 1;
                }
            }
            break :blk @Type(.{ .@"enum" = .{
                .tag_type = std.math.IntFittingRange(0, fields.len - 1),
                .fields = &fields,
                .decls = &.{},
                .is_exhaustive = true,
            } });
        };

        pub fn FunctionParams(comptime func: Function, comptime offset: comptime_int) type {
            const tmp = @typeInfo(@TypeOf(@field(T, @tagName(func)))).@"fn".params[offset..];
            if (tmp.len == 0) return void;

            var fields: [tmp.len]std.builtin.Type.StructField = undefined;

            for (tmp, &fields, 0..) |p, *field, i| {
                @setEvalBranchQuota(10_000);
                var num_buf: [128]u8 = undefined;
                field.* = .{
                    .name = std.fmt.bufPrintZ(&num_buf, "{d}", .{i}) catch unreachable,
                    .type = p.type orelse unreachable,
                    .default_value_ptr = null,
                    .alignment = 0,
                    .is_comptime = false,
                };
            }

            return @Type(.{ .@"struct" = .{
                .layout = .auto,
                .decls = &.{},
                .fields = &fields,
                .is_tuple = true,
            } });
        }

        pub fn FunctionReturn(comptime func: Function) type {
            return @typeInfo(@TypeOf(@field(T, @tagName(func)))).@"fn".return_type orelse unreachable;
        }

        pub fn FunctionReturnOverride(comptime func: Function, comptime Payload: type, comptime ExtraErrors: type) type {
            const info = @typeInfo(FunctionReturn(func));
            if (info == .error_union) {
                return @Type(.{
                    .error_union = .{
                        .error_set = info.error_union.error_set ++ @typeInfo(ExtraErrors).error_set,
                        .payload = Payload,
                    },
                });
            }
            return ExtraErrors!Payload;
        }

        pub fn ConstructorParams(comptime offset: comptime_int) type {
            if (@hasDecl(T, "init")) {
                return FunctionParams(.init, offset);
            }
            return void;
        }

        pub fn ConstructorReturn(comptime Payload: type, comptime ExtraErrors: type) type {
            if (@hasDecl(T, "init")) {
                return FunctionReturnOverride(.init, Payload, ExtraErrors);
            }
            return ExtraErrors!Payload;
        }

        pub const Flags = packed struct {
            need_gc: bool = false,
            can_destroy: bool = false,
        };

        var inst_id_lock: std.Thread.Mutex = .{};
        var inst_id: usize = 0;

        type_tag: []const u8 = @typeName(T),
        type_id: usize,
        allocator: Allocator,
        flags: Flags = .{},
        ref_lock: std.Thread.Mutex = .{},
        ref_count: usize = 0,
        value: T,

        pub fn init(self: *Self, alloc: Allocator, params: ConstructorParams(1)) ConstructorReturn(*Self, error{}) {
            inst_id_lock.lock();
            defer inst_id_lock.unlock();

            const id = inst_id;
            inst_id += 1;

            self.* = .{
                .type_id = id,
                .allocator = alloc,
                .value = undefined,
            };

            if (@hasDecl(T, "init")) {
                var full_params: ConstructorParams(0) = undefined;
                if (@TypeOf(params) != void) {
                    full_params[0] = &self.value;

                    inline for (std.meta.fields(@TypeOf(full_params))[1..], 0..) |field, i| {
                        @setEvalBranchQuota(10_000);
                        comptime var num_buf: [128]u8 = undefined;
                        @field(full_params, field.name) = @field(params, std.fmt.bufPrintZ(&num_buf, "{d}", .{i}) catch unreachable);
                    }
                }

                switch (@typeInfo(@typeInfo(@TypeOf(T.init)).@"fn".return_type.?)) {
                    .void => @call(.auto, T.init, full_params),
                    .error_union => try @call(.auto, T.init, full_params),
                    else => @compileError("Bad function return"),
                }
            }

            return self;
        }

        pub fn create(alloc: Allocator, params: ConstructorParams(1)) ConstructorReturn(*Self, Allocator.Error) {
            const self = try alloc.create(Self);
            errdefer alloc.destroy(self);

            const result = self.init(alloc, params);
            self.flags.can_destroy = true;
            return result;
        }

        pub fn ref(self: *Self) *Self {
            self.ref_lock.lock();
            defer self.ref_lock.unlock();
            self.ref_count += 1;
            return self;
        }

        pub fn call(self: *Self, comptime func: Function, params: FunctionParams(func, 1)) FunctionReturn(func) {
            var full_params: FunctionParams(func, 0) = undefined;
            if (@TypeOf(params) != void) {
                full_params[0] = &self.value;

                inline for (std.meta.fields(@TypeOf(full_params))[1..], 0..) |field, i| {
                    @setEvalBranchQuota(10_000);
                    comptime var num_buf: [128]u8 = undefined;
                    @field(full_params, field.name) = @field(params, std.fmt.bufPrintZ(&num_buf, "{d}", .{i}) catch unreachable);
                }
            }
            return @call(.auto, @field(T, @tagName(func)), full_params);
        }

        pub fn unref(self: *Self) void {
            self.ref_lock.lock();
            defer self.ref_lock.unlock();

            if (self.ref_count > 0) {
                self.ref_count -= 1;
                return;
            }

            if (!self.flags.need_gc) {
                return self.deinit();
            }
        }

        pub fn deinit(self: *Self) void {
            assert(self.ref_count == 0);
            if (@hasDecl(T, "deinit")) self.value.deinit(self.allocator);
            if (self.flags.can_destroy) self.allocator.destroy(self);
        }

        test {
            std.testing.refAllDeclsRecursive(T);
            std.testing.refAllDeclsRecursive(Self);
        }
    };
}
