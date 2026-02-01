const Token = @This();
pub const Idx = u32;

pub const Kind = enum(u8) {
    @"(", @")",
    @"{", @"}",
    @"[", @"]",

    @"+", @"-", @"*", @"/", @"%", @"=", @"!", @">", @"<", @"&", @"|", @"^", @".",
    @"==", @"!=", @">=", @"<=",
    @">>", @"<<",

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

const single_ch_tokens = blk: {
    @setEvalBranchQuota(1_000_000);

    var tokens: [128]Kind = undefined;
    @memset(&tokens, .invalid);

    for (@typeInfo(Kind).@"enum".fields) |field| {
        if (field.name.len != 1) continue;

        tokens[field.name[0]] = @enumFromInt(field.value);
    }

    break :blk tokens;
};

//for tokens made of multiple punctuation chars
//e.g. merges[.add][.equal] == .add_equal.
const merges = blk: {
    @setEvalBranchQuota(1_000_000);

    var m: std.EnumArray(Kind, std.EnumArray(Kind, Kind)) = .initFill(.initFill(.invalid));

    for (@typeInfo(Kind).@"enum".fields) |field| {
        const name = field.name;

        //token of lengt 2..3 made of punctuation?
        if (name.len > 3 or name.len < 2) continue;
        if (std.ascii.isAlphanumeric(name[0])) continue;

        const prev = name[0..name.len-1];
        const next = name[name.len-1..];

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

            else => single_ch_tokens[first_ch],

            0x80...0xff => @panic("todo: non ascii error"),
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
    try tokens.ensureTotalCapacity(gpa, src.len / 4);

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
        .{.@"(", "("},
        .{.@")", ")"},
        .{.ident, "ada_32"},
        .{.int, "0xadadAF9"},
        .{.float, "32.321"},
        .{.kw_and, "and"},
        .{.@"<", "<"},
        .{.@"=", "="},
        .{.@"==", "=="},
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
