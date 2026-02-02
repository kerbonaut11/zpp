const std = @import("std");
const Token = @import("Token.zig");
const Ir = @This();

pub const Ref = enum(u32) {
    none = 0,
    _,

    const min_non_primitive = std.enums.directEnumArrayLen(Ref, 0);
};

pub const Instr = union(enum(u8)) {
    pub const Kind = @typeInfo(@This()).@"union".tag_type.?;

    const BinaryOp = struct {
        lhs: Ref,
        rhs: Ref
    };

    const As = struct {
        type: Ref,
        val: Ref
    };

    const Block = struct {
        statement_idx: u32,
        statement_count: u32,
    };

    int: Token.Idx,

    bit_not: Ref, bool_not: Ref, neg: Ref,

    add: BinaryOp, sub: BinaryOp, div: BinaryOp, mul: BinaryOp, rem: BinaryOp,
    bit_and: BinaryOp, bit_or: BinaryOp, bit_xor: BinaryOp, shl: BinaryOp, shr: BinaryOp,
    bool_and: BinaryOp, bool_or: BinaryOp,

    block: Block,

    as: As,

    alloca: Ref,
};

instrs: std.MultiArrayList(Instr),
extra: std.ArrayList(u32),

pub inline fn instr(ir: *Ir, ref: Ref) Instr {
    return ir.instrs.get(@intFromEnum(ref) - Ref.min_non_primitive);
}

pub inline fn instrKind(ir: *Ir, ref: Ref) Instr.Kind {
    return ir.instrs.items(.tag)[@intFromEnum(ref) - Ref.min_non_primitive];
}
