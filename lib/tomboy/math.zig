pub fn signExtend(comptime T: type, value: T, bits: comptime_int) T {
    const shift = @sizeOf(T) * 8 - bits;
    return (@as(T, @intCast(value)) << shift) >> shift;
}
