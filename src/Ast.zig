const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("Token.zig");
const Ast = @This();
const Error = @import("Error.zig");
const Parser = @import("Parser.zig");

pub const Node = union(enum(u8)) {
    pub const Idx = u32;

    pub const UnaryOp = struct {
        pub fn precedence(op: Token.Kind) ?u8 {
            return switch (op) {
                .@"-", .@"!" => 0,
                .kw_not => BinaryOp.precedence(.kw_or).?-1,
                else => null,
            };
        }

        op_token: Token.Idx,
        val: Node.Idx,
    };

    pub const BinaryOp = struct {
        //lower precedence ops are evaluated first
        pub fn precedence(op: Token.Kind) ?u8 {
            return switch (op) {
                .@"*", .@"/", .@"%" => 1,
                .@"+", .@"-" => 2,
                .@"<<", .@">>" => 3,
                .@"&", .@"|", .@"^" => 4,
                .@"==", .@"!=", .@">", .@">=", .@"<", .@"<=" => 5,
                .kw_and, .kw_or => 7,
                else => null,
            };
        }

        op_token: Token.Idx,
        lhs: Token.Idx,
        rhs: Node.Idx,
    };

    pub const VarDecl = struct {
        main_token: Token.Idx,
        type: Node.Idx, //optional
        val: Node.Idx,
    };

    pub const FnDecl = struct {
        main_token: Token.Idx,
        extra: u32,
        body: Token.Idx,
    };

    pub const Block = struct {
        statement_idx: u32, //idx into extra
        statement_count: u32,
    };

    invalid,
    root: Node.Idx,

    int: Token.Idx,
    unary_op: UnaryOp,
    binary_op: BinaryOp,
    block: Block,

    var_decl: VarDecl,
    fn_decl: FnDecl,

};


src: []const u8,
tokens: std.MultiArrayList(Token),
nodes: std.MultiArrayList(Node),
extra: std.ArrayList(u32),
errors: std.ArrayList(Error),


inline fn tokenSrc(ast: *const Ast, token: Token.Idx) []const u8 {
    const loc = ast.tokens.items(.loc)[token];
    return ast.src[loc.start..loc.end];
}

pub const parse = Parser.parse;


pub fn dumpNode(ast: *const Ast, w: *std.io.Writer, node: Node.Idx, depth: u32) !void {
    try w.splatByteAll(' ', depth*2);

    switch (ast.nodes.get(node)) {
        .int => |token| try w.print("{s}\n", .{ast.tokenSrc(token)}),

        .unary_op => |op| {
            try w.print("{s} {{\n", .{ast.tokenSrc(op.op_token)});
            ast.dumpNode(w, op.val, depth+1) catch unreachable;
            try w.splatByteAll(' ', depth*2);
            try w.print("}}\n", .{});
        },

        .binary_op => |op| {
            try w.print("{s} {{\n", .{ast.tokenSrc(op.op_token)});
            try ast.dumpNode(w, op.lhs, depth+1);
            try ast.dumpNode(w, op.rhs, depth+1);
            try w.splatByteAll(' ', depth*2);
            try w.print("}}\n", .{});
        },

        .block => |block| {
            try w.print("{{\n", .{});
            for (ast.extra.items[block.statement_idx..][0..block.statement_count]) |i| {
                try ast.dumpNode(w, i, depth+1);
            }
            try w.splatByteAll(' ', depth*2);
            try w.print("}}\n", .{});
        },

        .var_decl => |decl| {
            try w.print("{s} {s} {{\n", .{ast.tokenSrc(decl.main_token), ast.tokenSrc(decl.main_token+1)});
            if (decl.type != 0) try ast.dumpNode(w, decl.type, depth+1);
            try ast.dumpNode(w, decl.val, depth+1);
            try w.splatByteAll(' ', depth*2);
            try w.print("}}\n", .{});
        },

        .fn_decl => |decl| {
            _ = decl;
            @panic("todo");
        },

        .invalid, .root => unreachable,
    }
}

pub fn dump(ast: Ast) !void {
    var buf: [64]u8 = undefined;
    const stdout = std.debug.lockStderrWriter(&buf);
    defer std.debug.unlockStdErr();
    try ast.dumpNode(stdout, ast.nodes.get(0).root, 0);
    try stdout.flush();
}

test {
    const src = "{let x: 1 = 34}";
    var ast = try parse(src, std.testing.allocator);
    defer ast.nodes.deinit(std.testing.allocator);
    defer ast.tokens.deinit(std.testing.allocator);
    defer ast.extra.deinit(std.testing.allocator);

    try ast.dump();
}

