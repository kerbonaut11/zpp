const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("Token.zig");
const Ast = @This();
const Error = @import("Error.zig");
const Parser = @import("Parser.zig");
pub const ExtraIdx = u32;

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
        //index into extra
        //[param count, param name token, param type, return type]
        proto: ExtraIdx,
        body: Token.Idx,

        pub fn params(func: FnDecl, ast: *const Ast) []struct{name: Token.Idx, type: Node.Idx} {
            const len = ast.extra.items[func.proto];
            return @ptrCast(ast.extra.items[func.proto+1..][0..len*2]);
        }

        pub fn returnType(func: FnDecl, ast: *const Ast) Node.Idx {
            const len = ast.extra.items[func.proto];
            return ast.extra.items[func.proto+1+len*2];
        }
    };

    pub const Block = struct {
        statement_idx: ExtraIdx,
        statement_count: u32,
    };

    invalid,
    root: Node.Idx,

    int: Token.Idx,
    bool: Token.Idx,
    float: Token.Idx,
    ident: Token.Idx,

    void_type: Token.Idx,
    bool_type: Token.Idx,

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


pub inline fn tokenSrc(ast: *const Ast, token: Token.Idx) []const u8 {
    const loc = ast.tokens.items(.loc)[token];
    return ast.src[loc.start..loc.end];
}

pub const parse = Parser.parse;

pub fn dump(ast: *const Ast) !void {
    var buf: [64]u8 = undefined;
    const stdout = std.debug.lockStderrWriter(&buf);
    defer std.debug.unlockStdErr();
    try @import("dump_ast.zig").dumpNode(ast, stdout, ast.nodes.get(0).root, "root", 0);
    try stdout.flush();
}

test {
    const src = "fn a(b: u32, c: u64) void {let x: 1 = 34}";
    var ast = try parse(src, std.testing.allocator);
    defer ast.nodes.deinit(std.testing.allocator);
    defer ast.tokens.deinit(std.testing.allocator);
    defer ast.extra.deinit(std.testing.allocator);

    try ast.dump();
}

