const Token = @import("Token.zig");
pub const Idx = u32;

pub const Cause = union(enum) {
    expected_expr,
    expected_tokens: []const Token.Kind,
};

first_token: Token.Idx,
last_token: Token.Idx,
cause: Cause,
