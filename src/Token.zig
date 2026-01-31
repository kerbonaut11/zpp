const Token = @This();

pub const Kind = enum(u8) {
    l_paren, r_paren,
    l_brace, r_brace,
    l_bracket, r_bracket,

    plus, minus, asterix, slash, equal, bang, greater, less, ampersand, pipe, carat,
    equal_equal, bang_equal, greater_equal, less_equal,

    ident,
    int, float,

    kw_and, kw_or, kw_not,
    kw_fn, kw_var, kw_let, kw_const,
    kw_void, kw_bool, kw_true, kw_false,

    eof,
    invalid,
};

pub const Loc = struct {
    start: u32,
    end: u32,
};

const keywords = blk: {
    const KVTuple = struct {[]const u8, Kind};
    var list: []const KVTuple = &.{};

    for (@typeInfo(Kind).@"enum".fields) |field| {
        if (std.mem.startsWith(u8, field.name, "kw_")) {
            list = list ++ .{.{field.name[3..], @as(Kind, @enumFromInt(field.value))}};
        }
    }

    break :blk std.StaticStringMap(Kind).initComptime(list);
};

//for tokens made of multiple punctuation chars
//e.g. merges[.add][.equal] == .add_equal.
const merges = blk: {
    @setEvalBranchQuota(1_000_000);

    var m: std.EnumArray(Kind, std.EnumArray(Kind, Kind)) = .initFill(.initFill(.invalid));

    for (@typeInfo(Kind).@"enum".fields) |field| {
        const split_idx = std.mem.lastIndexOfScalar(u8, field.name, '_') orelse continue;
        const prev = field.name[0..split_idx];
        const next = field.name[split_idx+1..];

        //ignore keywords and delimiters
        if (std.mem.eql(u8, prev, "kw")) continue;
        if (std.mem.eql(u8, prev, "l")) continue;
        if (std.mem.eql(u8, prev, "r")) continue;

        const prev_enum = std.meta.stringToEnum(Kind, prev) orelse @compileError("invalid token name:" ++ prev);
        const next_enum = std.meta.stringToEnum(Kind, next) orelse @compileError("invalid token name:" ++ next);

        m.getPtr(prev_enum).getPtr(next_enum).* = @enumFromInt(field.value);
    }

    break :blk m;
};


kind: Kind,
loc: Loc,

const Tokenizer = struct {
    src: []const u8,
    i: u32,

    fn nextCh(tokenizer: *Tokenizer) ?u8 {
        const ch = tokenizer.peekCh();
        tokenizer.i += 1;
        return ch;
    }

    fn peekCh(tokenizer: *Tokenizer) ?u8 {
        if (tokenizer.i >= tokenizer.src.len) return null;
        return tokenizer.src[tokenizer.i];
    }

    fn parseNext(tokenizer: *Tokenizer, out: *std.MultiArrayList(Token)) void {
        while (tokenizer.peekCh()) |ch| {
            if (!std.ascii.isWhitespace(ch)) break;
            tokenizer.i += 1;
        }

        const start = tokenizer.i;
        const first_ch = tokenizer.nextCh() orelse {
            out.appendAssumeCapacity(.{.kind = .eof, .loc = .{.start = 0, .end = 0}});
            return;
        };

        const kind: Kind = switch (first_ch) {
            '(' => .l_paren,
            ')' => .r_paren,
            '{' => .l_brace,
            '}' => .r_brace,
            '[' => .l_bracket,
            ']' => .r_bracket,

            '+' => .plus,
            '-' => .minus,
            '*' => .asterix,
            '/' => .slash,
            '>' => .greater,
            '<' => .less,
            '!' => .bang,
            '=' => .equal,

            'a'...'z', 'A'...'Z', '_' => blk: {
                while (tokenizer.peekCh()) |ch| {
                    if (!(std.ascii.isAlphanumeric(ch) or ch == '_')) break;
                    tokenizer.i += 1;
                }

                break :blk keywords.get(tokenizer.src[start..tokenizer.i]) orelse .ident;
            },

            '0'...'9' => blk: {
                var kind: Kind = .int;
                while (tokenizer.peekCh()) |ch| {
                    if (ch == '.' and kind == .int) {
                        kind = .float;
                    } else if (!std.ascii.isAlphanumeric(ch)) {
                        break;
                    }
                    tokenizer.i += 1;
                }
                break :blk kind;
            },

            else => unreachable,
        };

        const prev = if (out.len != 0) out.items(.kind)[out.len-1] else .invalid;
        const merge = merges.getPtrConst(prev).get(kind);
        if (merge != .invalid and out.get(out.len-1).loc.end == start) {
            const prev_token = out.pop().?;
            out.appendAssumeCapacity(.{.kind = merge, .loc = .{.start = prev_token.loc.start, .end = tokenizer.i}});
        } else {
            out.appendAssumeCapacity(.{.kind = kind, .loc = .{.start = start, .end = tokenizer.i}});
        }
    }
};

pub fn parse(src: []const u8, gpa: std.mem.Allocator) !std.MultiArrayList(Token) {
    var tokens = std.MultiArrayList(Token){};
    try tokens.ensureTotalCapacity(gpa, src.len);

    var tokenizer = Tokenizer{.src = src, .i = 0};

    tokenizer.parseNext(&tokens);
    while (tokens.len != 0 and tokens.items(.kind)[tokens.len-1] != .eof) {
        tokenizer.parseNext(&tokens);
    }

    return tokens;
}

const std = @import("std");

test {
    const src = 
        \\() ada_32 0xadadAF9 32.321 and < = ==
    ;

    const expected = [_]struct{Kind, []const u8}{
        .{.l_paren, "("},
        .{.r_paren, ")"},
        .{.ident, "ada_32"},
        .{.int, "0xadadAF9"},
        .{.float, "32.321"},
        .{.kw_and, "and"},
        .{.less, "<"},
        .{.equal, "="},
        .{.equal_equal, "=="},
        .{.eof, ""},
    };

    var tokens = try parse(src, std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    for (expected, 0..tokens.len) |expected_tk, i| {
        const tk = tokens.get(i);
        try std.testing.expectEqual(expected_tk[0], tk.kind);
        try std.testing.expectEqualSlices(u8, expected_tk[1], src[tk.loc.start..tk.loc.end]);
    }
}
