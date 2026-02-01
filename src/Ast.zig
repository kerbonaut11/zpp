const std = @import("std");
const Token = @import("Token.zig");
const Ast = @This();

pub const Node = union(enum(u8)) {
    pub const Idx = u32;
    pub const UnaryOp = struct {op_tk: Token.Idx};

    invalid,

    int: Token.Idx,

    unary_op: struct {
        op_token: Token.Idx,
        val: Node.Idx,
    },

    binary_op: struct {
        op_token: Token.Idx,
        lhs: Token.Idx,
        rhs: Node.Idx,
    },

    root: Node.Idx,
};

src: []const u8,
tokens: std.MultiArrayList(Token),
nodes: std.MultiArrayList(Node),

pub const UnaryOp = enum {
    neg,
    bit_not,
    bool_not,

    pub fn fromToken(token: Token.Kind) ?UnaryOp {
        return switch (token) {
            .minus => .neg,
            .bang => .bit_not,
            .kw_not => .bool_not,
            else => null,
        };
    }

    pub fn precedence(op: UnaryOp) u8 {
        return switch (op) {
            .neg, .bit_not => 0,
            .bool_not => BinaryOp.bool_or.precedence()-1,
        };
    }
};

pub const BinaryOp = enum {
    add, sub, mul, div, rem,
    bit_and, bit_or, bit_xor, shl, shr,
    bool_and, bool_or,
    eq, ne, gt, ge, lt, le,

    pub fn fromToken(token: Token.Kind) ?BinaryOp {
        return switch (token) {
            .plus => .add,
            .minus => .sub,
            .asterix => .mul,
            .slash => .div,
            .percent => .rem,

            .ampersand => .bit_and,
            .pipe => .bit_or,
            .carat => .bit_xor,
            .greater_greater => .shr,
            .less_less => .shl,

            .kw_and => .bool_and,
            .kw_or => .bool_or,

            .equal_equal => .eq,
            .bang_equal => .ne,
            .greater => .gt,
            .greater_equal => .ge,
            .less_equal => .lt,
            .less => .le,
            else => null,
        };
    }

    //lower precedence ops are evaluated first
    pub fn precedence(op: BinaryOp) u8 {
        return switch (op) {
            .mul, .div, .rem => 1,
            .add, .sub => 2,
            .shl, .shr => 3,
            .bit_and, .bit_or, .bit_xor => 4,
            .eq, .ne, .gt, .ge, .lt, .le => 5,
            .bool_and, .bool_or => 7,
        };
    }
};

inline fn getTokenSrc(ast: *Ast, token: Token.Idx) []const u8 {
    const loc = ast.tokens.items(.loc)[token];
    return ast.src[loc.start..loc.end];
}

const Parser = struct {
    gpa: std.mem.Allocator,
    ast: Ast,
    token_idx: Token.Idx,

    inline fn peekToken(parser: *Parser) Token {
        return parser.ast.tokens.get(parser.token_idx);
    }

    inline fn nextToken(parser: *Parser) Token {
        const token = parser.peekToken();
        if (token.kind != .eof) parser.token_idx += 1;
        return token;
    }

    inline fn advance(parser: *Parser) void {
        parser.token_idx += 1;
    }

    inline fn addNode(parser: *Parser, node: Node) Node.Idx {
        const idx: Node.Idx = @intCast(parser.ast.nodes.len);
        parser.ast.nodes.append(parser.gpa, node) catch {};
        return idx;
    }
    
    inline fn nextIfOpOfPrecedence(parser: *Parser, comptime Op: type, precedence: u8) bool {
        const op_kind = Op.fromToken(parser.peekToken().kind) orelse return false;
        if (op_kind.precedence() != precedence) return false;
        parser.advance();
        return true;
    }


    fn op(parser: *Parser, precedence: u8) Node.Idx {
        return if (UnaryOp.bit_not.precedence() == precedence or UnaryOp.bool_not.precedence() == precedence)
            parser.unaryOp(precedence)
        else
            parser.binaryOp(precedence);
    }

    fn lowerPrecedenceOp(parser: *Parser, precedence: u8) Node.Idx {
        return if (precedence != 0) parser.op(precedence-1) else baseExpr(parser);
    }

    fn unaryOp(parser: *Parser, precedence: u8) Node.Idx {
        const op_token = parser.token_idx;
        if (!parser.nextIfOpOfPrecedence(UnaryOp, precedence)) return parser.lowerPrecedenceOp(precedence);

        const val = parser.op(precedence);
        return parser.addNode(.{.unary_op = .{.op_token = op_token, .val = val}});
    }

    fn binaryOp(parser: *Parser, precedence: u8) Node.Idx {
        const lhs = parser.lowerPrecedenceOp(precedence);
        
        const op_token = parser.token_idx;
        if (!parser.nextIfOpOfPrecedence(BinaryOp, precedence)) return lhs;

        const rhs = parser.op(precedence);

        return parser.addNode(.{.binary_op = .{.op_token = op_token, .lhs = lhs, .rhs = rhs}});
    }

    fn baseExpr(parser: *Parser) Node.Idx {
        const first_token = parser.token_idx;
        return switch (parser.nextToken().kind) {
            .int => parser.addNode(.{.int = first_token}),
            else => unreachable,
        };
    }
};

pub fn parse(src: []const u8, gpa: std.mem.Allocator) !Ast {
    const tokens = try Token.parse(src, gpa);
    var parser = Parser{
        .gpa = gpa,
        .token_idx = 0,
        .ast = .{
            .src = src,
            .tokens = tokens,
            .nodes = .empty,
        },
    };

    try parser.ast.nodes.ensureTotalCapacity(gpa, tokens.len*2);
    _ = parser.addNode(.invalid);

    const root = parser.op(BinaryOp.bool_and.precedence());
    parser.ast.nodes.set(0, .{.root = root});

    return parser.ast;
}

pub fn dumpNode(ast: *Ast, w: *std.io.Writer, node: Node.Idx, depth: u32) !void {
    try w.splatByteAll(' ', depth*2);

    switch (ast.nodes.get(node)) {
        .int => |token| try w.print("{s}\n", .{ast.getTokenSrc(token)}),

        .unary_op => |op| {
            try w.print("{s} {{\n", .{ast.getTokenSrc(op.op_token)});
            ast.dumpNode(w, op.val, depth+1) catch unreachable;
            try w.splatByteAll(' ', depth*2);
            try w.print("}}\n", .{});
        },
        .binary_op => |op| {
            try w.print("{s} {{\n", .{ast.getTokenSrc(op.op_token)});
            try ast.dumpNode(w, op.lhs, depth+1);
            try ast.dumpNode(w, op.rhs, depth+1);
            try w.splatByteAll(' ', depth*2);
            try w.print("}}\n", .{});
        },
        .invalid, .root => unreachable,
    }
}
 
pub fn dump(ast: *Ast) !void {
    var buf: [64]u8 = undefined;
    const stdout = std.debug.lockStderrWriter(&buf);
    defer std.debug.unlockStdErr();
    try ast.dumpNode(stdout, ast.nodes.get(0).root, 0);
    try stdout.flush();
}

test {
    const src = "not 32 + 1*-2 == 3";
    var ast = try parse(src, std.testing.allocator);
    defer ast.nodes.deinit(std.testing.allocator);
    defer ast.tokens.deinit(std.testing.allocator);

    try ast.dump();
}
