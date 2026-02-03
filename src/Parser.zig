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
extra_idx: Ast.ExtraIdx,

inline fn peek(p: *Parser) Token.Kind {
    return p.ast.tokens.items(.kind)[p.token_idx];
}

inline fn next(p: *Parser) Token.Kind {
    const token = p.peek();
    if (token != .eof) p.token_idx += 1;
    return token;
}

inline fn nextIf(p: *Parser, expected: Token.Kind) bool {
    if (p.peek() == expected) {
        p.advance();
        return true;
    }

    return false;
}

inline fn nextExpect(p: *Parser, comptime expected: Token.Kind) !void {
    _ = try nextExpectAny(p, &.{expected});
}

inline fn nextExpectAny(p: *Parser, comptime expected: []const Token.Kind) !Token.Kind {
    if (std.mem.indexOfScalar(Token.Kind, expected, p.peek()) == null) {
        return p.addError(.{
            .cause = .{.expected_tokens = expected},
            .first_token = p.token_idx,
            .last_token = p.token_idx,
        });
    }

    return p.next();
}

inline fn advance(p: *Parser) void {
    p.token_idx += 1;
}

inline fn back(p: *Parser) void {
    p.token_idx -= 1;
}

inline fn addNode(p: *Parser, node: Node) Allocator.Error!Node.Idx {
    const idx: Node.Idx = @intCast(p.ast.nodes.len);
    try p.ast.nodes.append(p.gpa, node);
    return idx;
}

inline fn addExtra(p: *Parser, data: u32) Allocator.Error!void {
    p.extra_idx += 1;
    try p.ast.extra.append(p.gpa, data);
}

fn addError(p: *Parser, err: Error) ErrorSet {
    try p.ast.errors.append(p.gpa, err);
    return error.ParseFailed;
}

fn expr(p: *Parser) ErrorSet!Node.Idx {
    return p.op(7);
}

fn op(p: *Parser, precedence: u8) ErrorSet!Node.Idx {
    return if (Node.UnaryOp.precedence(.@"-") == precedence or Node.UnaryOp.precedence(.kw_not) == precedence)
        p.unaryOp(precedence)
    else
        p.binaryOp(precedence);
}

fn lowerPrecedenceOp(p: *Parser, precedence: u8) ErrorSet!Node.Idx {
    return if (precedence != 0) p.op(precedence-1) else baseExpr(p);
}

fn unaryOp(p: *Parser, precedence: u8) ErrorSet!Node.Idx {
    const op_token = p.token_idx;
    if (Node.UnaryOp.precedence(p.peek()) != precedence) return p.lowerPrecedenceOp(precedence);
    p.advance();

    const val = try p.op(precedence);
    return p.addNode(.{.unary_op = .{.op_token = op_token, .val = val}});
}

fn binaryOp(p: *Parser, precedence: u8) ErrorSet!Node.Idx {
    const lhs = try p.lowerPrecedenceOp(precedence);
    
    const op_token = p.token_idx;
    if (Node.BinaryOp.precedence(p.peek()) != precedence) return lhs;
    p.advance();

    const rhs = try p.op(precedence);

    return p.addNode(.{.binary_op = .{.op_token = op_token, .lhs = lhs, .rhs = rhs}});
}

fn baseExpr(p: *Parser) ErrorSet!Node.Idx {
    const first_token = p.token_idx;

    return switch (p.peek()) {
        .int   => p.singleTokenNode("int"),
        .float => p.singleTokenNode("float"),
        .ident => p.singleTokenNode("ident"),
        .kw_true, .kw_false => p.singleTokenNode("bool"),

        .kw_void => p.singleTokenNode("void_type"),
        .kw_bool => p.singleTokenNode("bool_type"),

        .@"{" => return p.block(),

        else => {
            const err = Error{
                .cause = .expected_expr,
                .first_token = first_token,
                .last_token = first_token,
            };
            std.debug.print("{}\n", .{p.peek()});
            return p.addError(err);
        },
    };
}

pub fn singleTokenNode(p: *Parser, comptime name: []const u8) Allocator.Error!Node.Idx {
    const idx = p.token_idx;
    p.advance();
    return p.addNode(@unionInit(Node, name, idx));
}

fn block(p: *Parser) ErrorSet!Node.Idx {
    try p.nextExpect(.@"{");

    const statement_idx: u32 = p.extra_idx;
    var statement_count: u32 = 0;

    while (true) {
        switch (p.peek()) {
            .kw_var, .kw_let => try p.addExtra(try p.varDecl()),

            else => try p.addExtra(try p.expr()),

            .@"}" => break,
        }

        statement_count += 1;
    }

    return p.addNode(.{.block = .{.statement_idx = statement_idx, .statement_count = statement_count}});
}

fn varDecl(p: *Parser) ErrorSet!Node.Idx {
    const main_token = p.token_idx;
    _ = try p.nextExpectAny(&.{.kw_var, .kw_let});
    try p.nextExpect(.ident);

    const @"type" = if (p.nextIf(.@":")) try p.expr() else 0;

    try p.nextExpect(.@"=");
    const val = try p.expr();
    
    return p.addNode(.{.var_decl = .{
        .main_token = main_token,
        .type = @"type",
        .val = val,
    }});
}

fn fnDecl(p: *Parser) ErrorSet!Node.Idx {
    const proto_idx: Ast.ExtraIdx = p.extra_idx;
    try p.addExtra(0); //parameter count
    const main_token = p.token_idx;

    try p.nextExpect(.kw_fn);
    try p.nextExpect(.ident);
    try p.nextExpect(.@"(");

    while (true) {
        if (p.nextIf(.@")")) break;

        try p.addExtra(p.token_idx);
        try p.nextExpect(.ident);
        try p.nextExpect(.@":");
        try p.addExtra(try p.expr());

        p.ast.extra.items[proto_idx] += 1;
        if (try p.nextExpectAny(&.{.@")", .@","}) == .@")") break;
    }

    try p.addExtra(try p.expr());

    const body = try p.block();
    
    return p.addNode(.{.fn_decl = .{
        .main_token = main_token,
        .proto = proto_idx,
        .body = body,
    }});
}


pub fn parse(src: []const u8, gpa: Allocator) !Ast {
    const tokens = try Token.parse(src, gpa);
    var p = Parser{
        .gpa = gpa,
        .token_idx = 0,
        .extra_idx = 0,
        .ast = .{
            .src = src,
            .tokens = tokens,
            .nodes = .empty,
            .extra = .empty,
            .errors = .empty,
        },
    };

    try p.ast.nodes.ensureTotalCapacity(gpa, tokens.len*2);
    _ = try p.addNode(.invalid);

    const root = try p.fnDecl();
    p.ast.nodes.set(0, .{.root = root});

    return p.ast;
}
