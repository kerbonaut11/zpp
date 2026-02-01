const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("Token.zig");
const Ast = @import("Ast.zig");
const Error = @import("Error.zig");
const Parser = @This();
const Node = Ast.Node;
pub const ErrorSet = error {ParseFailed} || Allocator.Error;

gpa: Allocator,
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

inline fn nextExpect(parser: *Parser, expected: Token.Kind) !void {
    if (parser.peekToken().kind != expected) {
        return parser.addError(.{
            .cause = .{.expected_token = expected},
            .first_token = parser.token_idx,
            .last_token = parser.token_idx,
        });
    }

    parser.advance();
}

inline fn advance(parser: *Parser) void {
    parser.token_idx += 1;
}

inline fn back(parser: *Parser) void {
    parser.token_idx -= 1;
}

inline fn addNode(parser: *Parser, node: Node) Allocator.Error!Node.Idx {
    const idx: Node.Idx = @intCast(parser.ast.nodes.len);
    try parser.ast.nodes.append(parser.gpa, node);
    return idx;
}

inline fn addExtraData(parser: *Parser, data: u32) Allocator.Error!void {
    try parser.ast.extra_data.append(parser.gpa, data);
}

fn addError(parser: *Parser, err: Error) ErrorSet {
    try parser.ast.errors.append(parser.gpa, err);
    return error.ParseFailed;
}

fn expr(parser: *Parser) ErrorSet!Node.Idx {
    return parser.op(7);
}

fn op(parser: *Parser, precedence: u8) ErrorSet!Node.Idx {
    return if (Node.UnaryOp.precedence(.@"-") == precedence or Node.UnaryOp.precedence(.kw_not) == precedence)
        parser.unaryOp(precedence)
    else
        parser.binaryOp(precedence);
}

fn lowerPrecedenceOp(parser: *Parser, precedence: u8) ErrorSet!Node.Idx {
    return if (precedence != 0) parser.op(precedence-1) else baseExpr(parser);
}

fn unaryOp(parser: *Parser, precedence: u8) ErrorSet!Node.Idx {
    const op_token = parser.token_idx;
    if (Node.UnaryOp.precedence(parser.peekToken().kind) != precedence) return parser.lowerPrecedenceOp(precedence);
    parser.advance();

    const val = try parser.op(precedence);
    return parser.addNode(.{.unary_op = .{.op_token = op_token, .val = val}});
}

fn binaryOp(parser: *Parser, precedence: u8) ErrorSet!Node.Idx {
    const lhs = try parser.lowerPrecedenceOp(precedence);
    
    const op_token = parser.token_idx;
    if (Node.BinaryOp.precedence(parser.peekToken().kind) != precedence) return lhs;
    parser.advance();

    const rhs = try parser.op(precedence);

    return parser.addNode(.{.binary_op = .{.op_token = op_token, .lhs = lhs, .rhs = rhs}});
}

fn baseExpr(parser: *Parser) ErrorSet!Node.Idx {
    const first_token = parser.token_idx;

    switch (parser.peekToken().kind) {
        .int => {
            parser.advance();
            return parser.addNode(.{.int = first_token});
        },

        .@"{" => return parser.block(),

        else => {
            const err = Error{
                .cause = .expected_expr,
                .first_token = first_token,
                .last_token = first_token,
            };
            std.debug.print("{}\n", .{parser.peekToken()});
            return parser.addError(err);
        },
    }
}

fn block(parser: *Parser) ErrorSet!Node.Idx {
    try parser.nextExpect(.@"{");

    const statement_idx: u32 = @intCast(parser.ast.extra_data.items.len);
    var statement_count: u32 = 0;

    while (true) {
        switch (parser.peekToken().kind) {
            .kw_var, .kw_let => try parser.addExtraData(try parser.varDecl()),

            else => try parser.addExtraData(try parser.expr()),

            .@"}" => break,
        }

        statement_count += 1;
    }

    return parser.addNode(.{.block = .{.statement_idx = statement_idx, .statement_count = statement_count}});
}

fn varDecl(parser: *Parser) ErrorSet!Node.Idx {
    _ = parser;
    @panic("todo");
}


pub fn parse(src: []const u8, gpa: Allocator) !Ast {
    const tokens = try Token.parse(src, gpa);
    var parser = Parser{
        .gpa = gpa,
        .token_idx = 0,
        .ast = .{
            .src = src,
            .tokens = tokens,
            .nodes = .empty,
            .extra_data = .empty,
            .errors = .empty,
        },
    };

    try parser.ast.nodes.ensureTotalCapacity(gpa, tokens.len*2);
    _ = try parser.addNode(.invalid);

    const root = try parser.op(Node.BinaryOp.precedence(.kw_and).?);
    parser.ast.nodes.set(0, .{.root = root});

    return parser.ast;
}
