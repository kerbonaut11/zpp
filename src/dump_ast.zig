const std = @import("std");
const Ast = @import("Ast.zig");
const Node = Ast.Node;

pub fn printCloseBrace(w: *std.Io.Writer, depth: u32) !void {
    try w.splatByteAll(' ', depth*2);
    try w.print("}}\n", .{});
}

pub fn dumpNode(ast: *const Ast, w: *std.Io.Writer, node: Node.Idx, label: []const u8, depth: u32) !void {
    try w.splatByteAll(' ', depth*2);
    if (label.len != 0) {
        try w.print("{s}: ", .{label});
    }

    switch (ast.nodes.get(node)) {
        .int => |token| try w.print("{s}\n", .{ast.tokenSrc(token)}),

        .unary_op => |op| {
            try w.print("{s} {{\n", .{ast.tokenSrc(op.op_token)});
            dumpNode(ast, w, op.val, "", depth+1) catch unreachable;
            try printCloseBrace(w, depth);
        },

        .binary_op => |op| {
            try w.print("{s} {{\n", .{ast.tokenSrc(op.op_token)});
            try dumpNode(ast, w, op.lhs, "lhs", depth+1);
            try dumpNode(ast, w, op.rhs, "rhs", depth+1);
            try printCloseBrace(w, depth);
        },

        .block => |block| {
            try w.print("{{\n", .{});
            for (ast.extra.items[block.statement_idx..][0..block.statement_count], 0..) |idx, n| {
                var buf: [8]u8 = undefined;
                try dumpNode(ast, w, idx, try std.fmt.bufPrint(&buf, "{}", .{n}), depth+1);
            }
            try printCloseBrace(w, depth);
        },

        .var_decl => |decl| {
            try w.print("{s} {s} {{\n", .{ast.tokenSrc(decl.main_token), ast.tokenSrc(decl.main_token+1)});
            if (decl.type != 0) try dumpNode(ast, w, decl.type, "type", depth+1);
            try dumpNode(ast, w, decl.val, "val", depth+1);
            try printCloseBrace(w, depth);
        },

        .fn_decl => |decl| {
            try w.print("{s} {s} {{\n", .{ast.tokenSrc(decl.main_token), ast.tokenSrc(decl.main_token+1)});
            for (decl.params(ast)) |param| {
                try dumpNode(ast, w, param.type, ast.tokenSrc(param.type), depth+1);
            }
            try dumpNode(ast, w, decl.returnType(ast), "return", depth+1);
            try dumpNode(ast, w, decl.body, "body", depth+1);
            try printCloseBrace(w, depth);
        },

        .invalid, .root => unreachable,
    }
}
