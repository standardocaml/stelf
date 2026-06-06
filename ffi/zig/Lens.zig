const path = []const u8;

const Lens = struct {
    fn Loc(comptime _: type) type {
        return union(enum) { ghost, loc: .{ path, i32, i32 } };
    }
    fn LTerm(comptime T: type) type {
        const loc = Loc(T);
        const symbol = []const u8;
        return union(enum) { lowercase: .{ loc, symbol }, uppercase: .{ loc, symbol } }; // TODO Finsish adding
        //
    }
};
