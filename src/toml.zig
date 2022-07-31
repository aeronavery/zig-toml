const std = @import("std");
const assert = std.debug.assert;
const fmt = std.fmt;

const DottedIdentifier = std.TailQueue([]const u8);

pub const Key = union(enum) {
    None,
    DottedIdent: DottedIdentifier,
    Ident: []const u8,

    pub fn deinit(key: *Key, allocator: *std.mem.Allocator) void {
        if (key.* == .DottedIdent) {
            var it = key.DottedIdent;
            while (it.pop()) |node| {
                allocator.destroy(node);
            }
        }
    }
};

pub const DynamicArray = std.ArrayList(Value);
pub const TableArray = std.ArrayList(*Table);

pub const Value = union(enum) {
    None,
    String: []const u8,
    Boolean: bool,
    Integer: i64,
    Float: f64,
    Array: DynamicArray,
    Table: *Table,
    ManyTables: TableArray,

    pub fn isTable(self: Value) bool {
        return self == .Table;
    }

    pub fn isManyTables(self: Value) bool {
        return self == .ManyTables;
    }

    pub fn deinit(self: *Value) void {
        switch (self.*) {
            .Array => |*array| {
                for (array.items) |*item| {
                    item.deinit();
                }
                array.deinit();
            },
            .Table => |table| {
                table.deinit();
            },
            .ManyTables => |tables| {
                for (tables.items) |table| {
                    table.deinit();
                }
                tables.deinit();
            },
            else => {},
        }
    }
};

pub const Table = struct {
    const Self = @This();

    const Error = error{ table_is_one, key_already_exists, expected_table_of_one };
    const KeyMap = std.StringHashMap(Value);

    keys: KeyMap,
    name: []const u8,

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, name: []const u8) Self {
        return Self{
            .keys = KeyMap.init(allocator),
            .name = name,
            .allocator = allocator,
        };
    }

    pub fn create(allocator: std.mem.Allocator, name: []const u8) !*Self {
        var result = try allocator.create(Table);
        result.* = Table.init(allocator, name);
        return result;
    }

    /// Cleans up the table's keys and its children
    pub fn deinit(self: *Self) void {
        var it = self.keys.iterator();
        while (it.next()) |node| {
            node.value_ptr.*.deinit();
        }
        self.keys.deinit();
        self.allocator.destroy(self);
    }

    fn indexIdentifier(ident: DottedIdentifier, index: usize) ?[]const u8 {
        if (index >= ident.len) {
            return null;
        }

        var it = ident.first;
        var current: []const u8 = undefined;
        var i = index;
        while (it) |node| : (it = node.next) {
            current = node.data;
            if (i == 0) {
                break;
            }
            i -= 1;
        }

        return current;
    }

    pub fn addKey(self: *Self, key: Key, value: Value) !void {
        switch (key) {
            Key.None => {
                return;
            },
            Key.Ident => |name| {
                var old = try self.keys.fetchPut(name, value);
                if (old) |_| {
                    return Self.Error.key_already_exists;
                }
            },
            Key.DottedIdent => |dotted| {
                var current_table: *Table = self;
                var index: usize = 0;
                while (index < dotted.len - 1) : (index += 1) {
                    if (current_table.keys.get(indexIdentifier(dotted, index).?)) |pair| {
                        if (pair.isManyTables()) {
                            return Self.Error.expected_table_of_one;
                        }
                        current_table = pair.Table;
                    } else {
                        var table = try self.allocator.create(Table);
                        table.* = Table.init(self.allocator, indexIdentifier(dotted, index).?);
                        try current_table.addTable(table);
                        current_table = table;
                    }
                }
                var old = try current_table.keys.fetchPut(indexIdentifier(dotted, index).?, value);
                if (old) |_| {
                    return Self.Error.key_already_exists;
                }
            },
        }
    }

    pub fn addTable(self: *Self, table: *Table) !void {
        if (self.keys.get(table.name)) |_| {
            return Self.Error.key_already_exists;
        }
        _ = try self.keys.put(table.name, Value{ .Table = table });
    }

    pub fn addManyTable(self: *Self, table: *Table) !void {
        if (self.keys.getPtr(table.name)) |pair| {
            if (pair.isManyTables()) {
                try pair.ManyTables.append(table);
            } else {
                return Self.Error.table_is_one;
            }
        } else {
            var value = TableArray.init(self.allocator);
            try value.append(table);
            var old = try self.keys.fetchPut(table.name, Value{ .ManyTables = value });
            // since we already tested if there's a table then this should be unreachable
            if (old) |_| {
                unreachable;
            }
        }
    }

    pub fn addNewTable(self: *Self, name: []const u8) !*Table {
        var table = try Table.create(self.allocator, name);
        var old = try self.keys.fetchPut(name, Value{ .Table = table });
        if (old) |_| {
            return Self.Error.key_already_exists;
        }
        return table;
    }
};

fn isEof(c: u8) bool {
    return c == 0;
}

fn isIdentifier(c: u8) bool {
    return (c >= 65 and c <= 90) or (c >= 48 and c <= 57) or (c >= 97 and c <= 122) or c == '-' or c == '_';
}

fn isQuote(c: u8) bool {
    return c == '"' or c == '\'';
}

fn isWhitespace(c: u8) bool {
    return c == '\n' or c == '\t' or c == ' ' or c == '\r';
}

/// denotes whitespace that is allowed between a key and its value
fn isPairWhitespace(c: u8) bool {
    return c == '\t' or c == ' ';
}

fn isNumber(word: []const u8) bool {
    var i: usize = 0;
    if (word[i] == '_') {
        return false;
    }
    if (word[i] == '-' or word[i] == '+' or word[i] == '.') {
        i += 1;
    }
    while (i < word.len) : (i += 1) {
        var c = word[i];
        if (c == '_') {
            if (i + 1 >= word.len) {
                return false;
            }
            i += 1;
            c = word[i];
        } else if (c == '.') {
            i += 1;
            c = word[i];
        }
        if (!(c >= 48 and c <= 57)) {
            return false;
        }
    }
    return true;
}

fn isFloat(word: []const u8) bool {
    var i: usize = 0;
    while (i < word.len) : (i += 1) {
        var c = word[i];
        if (c == '.') {
            return true;
        }
    }
    return false;
}

fn toFloat(word: []const u8) f64 {
    // var result: f64 = 0;
    var result = fmt.parseFloat(f64, word) catch -1.2345;
    return result;
}

fn toInteger(word: []const u8) i64 {
    var result: i64 = 0;
    var i: usize = 0;
    var negative = false;
    if (word[i] == '-') {
        negative = true;
        i += 1;
    } else if (word[i] == '+') {
        i += 1;
    }
    while (true) {
        if (word[i] == '_') {
            i += 1;
            continue;
        }
        result += @intCast(i64, (word[i] - 48));
        i += 1;
        if (i < word.len) {
            result *= 10;
        } else {
            break;
        }
    }
    if (negative) {
        result *= -1;
    }
    return result;
}

pub const Parser = struct {
    const Error = error{
        unexpected_eof,
        expected_identifier,
        expected_equals,
        expected_newline,
        expected_closing_brace,
        malformed_table,
        expected_comma,
        invalid_value,
        unexpected_newline,
    };

    const Pair = struct {
        key: Key,
        value: Value,
    };

    allocator: std.mem.Allocator,
    global_table: *Table,
    // denotes if contents have been heap allocated (from a file)
    allocated: bool,
    filename: []const u8,
    contents: []const u8,
    line: usize,
    column: usize,
    index: usize,

    pub fn initWithFile(allocator: std.mem.Allocator, filename: []const u8) !Parser {
        var contents = try std.fs.cwd().readFileAlloc(allocator, filename, std.math.maxInt(usize));
        var parser = try Parser.initWithString(allocator, contents);
        parser.filename = filename;
        parser.allocated = true;
        return parser;
    }

    pub fn initWithString(allocator: std.mem.Allocator, str: []const u8) !Parser {
        return Parser{
            .allocator = allocator,
            .global_table = try Table.create(allocator, ""),
            .allocated = false,
            .filename = "",
            .contents = str,
            .line = 1,
            .column = 0,
            .index = 0,
        };
    }

    pub fn deinit(self: *Parser) void {
        if (self.allocated) {
            self.allocator.free(self.contents);
        }
    }

    fn rawNextChar(self: *Parser) u8 {
        if (self.index >= self.contents.len) {
            return 0;
        }

        var c = self.contents[self.index];
        if (c == '\n') {
            self.line += 1;
            self.column = 0;
        }
        self.index += 1;
        self.column += 1;
        return c;
    }

    fn nextChar(self: *Parser) u8 {
        var c = self.rawNextChar();
        // Skip any comments
        while (c == '#') {
            c = self.rawNextChar();
            while (!isEof(c)) {
                if (c == '\n') {
                    break;
                }
                c = self.rawNextChar();
            }

            if (isEof(c)) {
                return 0;
            }
        }

        return c;
    }

    fn curChar(self: Parser) u8 {
        if (self.index == 0) {
            return self.contents[self.index];
        } else if (self.index >= self.contents.len) {
            return self.contents[self.contents.len - 1];
        } else {
            return self.contents[self.index - 1];
        }
    }

    fn peekChar(self: Parser) u8 {
        if (self.index >= self.contents.len) {
            return 0;
        }

        return self.contents[self.index];
    }

    fn getIndex(self: Parser) usize {
        if (self.index == 0) {
            return 0;
        }
        return self.index - 1;
    }

    fn nextCharIgnoreWhitespace(self: *Parser) u8 {
        var c = self.nextChar();
        while (isWhitespace(c)) {
            c = self.nextChar();
        }
        return c;
    }

    fn ignoreWhitespace(self: *Parser) u8 {
        var c = self.curChar();
        while (isWhitespace(c)) {
            c = self.nextChar();
        }
        return c;
    }

    fn isNewline(self: *Parser, c: u8) bool {
        var n = c;
        if (n == '\r') {
            n = self.peekChar();
        }
        if (n == '\n') {
            return true;
        }
        return false;
    }

    // parses `contents` and returns the global table
    pub fn parse(self: *Parser) !*Table {
        try self.parseTable(self.global_table);
        return self.global_table;
    }

    fn parseTable(self: *Parser, table: *Table) anyerror!void {
        var has_newline = true;
        var c = self.nextChar();
        while (!isEof(c)) {
            c = self.ignoreWhitespace();

            if (!has_newline) {
                return Parser.Error.expected_newline;
            }
            has_newline = false;

            // parse table
            if (c == '[') {
                var is_array = false;
                if (self.peekChar() == '[') {
                    c = self.nextChar();
                    is_array = true;
                }
                c = self.nextChar();

                var table_key = try self.parseKeyIdentifier();
                defer table_key.deinit(&self.allocator);
                c = self.curChar();

                if (c != ']') {
                    return Parser.Error.expected_closing_brace;
                }
                if (is_array) {
                    c = self.nextChar();
                    if (c != ']') {
                        return Parser.Error.expected_closing_brace;
                    }
                }

                var table_name: []const u8 = undefined;
                var current_table = self.global_table;
                switch (table_key) {
                    Key.None => {
                        return Parser.Error.malformed_table;
                    },
                    Key.Ident => |ident| {
                        table_name = ident;
                    },
                    Key.DottedIdent => |dotted| {
                        // iterate through the identifiers and create any missing tables along the way
                        var it = dotted.first;
                        while (it) |node| : (it = node.next) {
                            if (node == dotted.last) {
                                break;
                            }
                            if (current_table.keys.get(node.data)) |pair| {
                                if (pair.isManyTables()) {
                                    return Parser.Error.malformed_table;
                                }
                                current_table = pair.Table;
                            } else {
                                current_table = try current_table.addNewTable(node.data);
                            }
                        }
                        table_name = dotted.last.?.data;
                    },
                }

                var new_table = try Table.create(self.allocator, table_name);
                // add before parsing so then if adding returns an error we get the proper line/column
                if (is_array) {
                    try current_table.addManyTable(new_table);
                } else {
                    try current_table.addTable(new_table);
                }
                try self.parseTable(new_table);

                // If there's a new table then this table can't have any more pairs
                // Even if this break was removed no pairs would be parsed and we would end up with an error from parsePair
                break;
            } else {

                // if is eof after table declaration then stop parsing table
                if (isEof(c)) {
                    break;
                }
                var pair = try self.parsePair();
                defer pair.key.deinit(&self.allocator);
                try table.addKey(pair.key, pair.value);

                c = self.curChar();
                // ignore whitespace after pair
                while (isPairWhitespace(c)) {
                    c = self.nextChar();
                }

                if (self.isNewline(c)) {
                    has_newline = true;
                }
            }
            c = self.nextChar();
        }
    }

    fn parsePair(self: *Parser) !Pair {
        var c = self.curChar();
        if (isEof(c)) {
            return Parser.Error.expected_identifier;
        }

        var key = try self.parseKeyIdentifier();

        // ignore whitespace before equals
        c = self.curChar();
        while (isPairWhitespace(c)) {
            c = self.nextChar();
        }

        if (c != '=') {
            return Parser.Error.expected_equals;
        }

        c = self.nextChar();
        // ignore whitespace after equals
        while (isPairWhitespace(c)) {
            c = self.nextChar();
        }

        var value = try self.parseValue();

        return Pair{
            .key = key,
            .value = value,
        };
    }

    fn convertIdentifierToValue(word: []const u8) !Value {
        if (std.mem.eql(u8, word, "true")) {
            return Value{ .Boolean = true };
        } else if (std.mem.eql(u8, word, "false")) {
            return Value{ .Boolean = false };
        } else if (isNumber(word)) {
            if (isFloat(word)) {
                return Value{ .Float = toFloat(word) };
            }
            return Value{ .Integer = toInteger(word) };
        } else {
            return Parser.Error.invalid_value;
        }
    }

    fn parseValue(self: *Parser) anyerror!Value {
        var c = self.curChar();
        if (isQuote(c)) {
            return Value{ .String = try self.parseString(c) };
        }

        // array
        if (c == '[') {
            return try self.parseArray();
        }

        // inline table
        if (c == '{') {
            return try self.parseInlineTable();
        }

        var ident = try self.parseIdentifier();
        return try Parser.convertIdentifierToValue(ident);
    }

    fn parseArray(self: *Parser) anyerror!Value {
        var result = DynamicArray.init(self.allocator);
        var c = self.nextChar();

        var has_comma = true;
        while (c != ']' and !isEof(c)) {
            if (!has_comma) {
                return Parser.Error.expected_comma;
            }
            has_comma = false;

            c = self.ignoreWhitespace();

            var value = try self.parseValue();
            try result.append(value);

            c = self.ignoreWhitespace();
            if (c == ',') {
                c = self.nextCharIgnoreWhitespace();
                has_comma = true;
            } else if (c != ']') {
                return Parser.Error.expected_closing_brace;
            }
        }

        if (isEof(c)) {
            return Parser.Error.unexpected_eof;
        }

        // eat the ]
        _ = self.nextChar();

        return Value{ .Array = result };
    }

    fn parseInlineTable(self: *Parser) anyerror!Value {
        var result = try Table.create(self.allocator, "");

        var c = self.nextChar();

        var has_comma = true;
        while (c != '}' and !isEof(c)) {
            if (!has_comma) {
                return Parser.Error.expected_comma;
            }
            has_comma = false;

            if (self.isNewline(c)) {
                return Parser.Error.unexpected_newline;
            }

            while (isPairWhitespace(c)) {
                c = self.nextChar();
            }

            var pair = try self.parsePair();
            try result.addKey(pair.key, pair.value);

            c = self.curChar();
            // ignore all whitespace that is allowed after a pair
            // then check for a newline
            while (isPairWhitespace(c)) {
                c = self.nextChar();
            }

            if (self.isNewline(c)) {
                return Parser.Error.unexpected_newline;
            }

            if (c == ',') {
                // grab the next char ignoring any pair whitespace
                c = self.nextChar();
                while (isPairWhitespace(c)) {
                    c = self.nextChar();
                }
                has_comma = true;
            }
        }

        if (isEof(c)) {
            return Parser.Error.unexpected_eof;
        }

        _ = self.nextChar();

        return Value{ .Table = result };
    }

    fn parseWord(self: *Parser) ![]const u8 {
        var c = self.curChar();
        if (isQuote(c)) {
            return try self.parseString(c);
        }
        if (isIdentifier(c)) {
            return try self.parseIdentifier();
        }
        return Parser.Error.expected_identifier;
    }

    fn parseKeyIdentifier(self: *Parser) !Key {
        var keyValue = try self.parseWord();

        var c = self.curChar();
        if (c == '.') {
            var dottedResult = try self.parseDottedIdentifier();

            var node = try self.allocator.create(DottedIdentifier.Node);
            node.data = keyValue;

            dottedResult.prepend(node);
            return Key{ .DottedIdent = dottedResult };
        } else {
            return Key{ .Ident = keyValue };
        }
    }

    // expects self.curChar() to be a .
    fn parseDottedIdentifier(self: *Parser) !DottedIdentifier {
        var result = DottedIdentifier{};

        var c = self.curChar();
        while (c == '.') {
            var ident: []const u8 = undefined;
            c = self.nextChar();
            if (isQuote(c)) {
                ident = try self.parseString(c);
            } else {
                ident = try self.parseIdentifier();
            }

            var node = try self.allocator.create(DottedIdentifier.Node);
            node.data = ident;

            result.append(node);
            c = self.curChar();
        }

        return result;
    }

    fn parseString(self: *Parser, opening: u8) ![]const u8 {
        var c = self.rawNextChar();
        var start = self.getIndex();
        while (c != opening and !isEof(c)) {
            c = self.rawNextChar();
        }

        if (isEof(c)) {
            return Parser.Error.unexpected_eof;
        }

        // eat the closing quote
        var ending = self.getIndex();
        c = self.nextChar();

        return self.contents[start..ending];
    }

    fn parseIdentifier(self: *Parser) ![]const u8 {
        var start = self.getIndex();
        var c = self.nextChar();
        while (isIdentifier(c) and !isEof(c)) {
            c = self.nextChar();

            if (isNumber(self.contents[start..self.getIndex()])) {
                if (self.contents[self.getIndex()] == '.') {
                    // it's a float
                    c = self.nextChar();
                }
            }
        }

        if (isEof(c)) {
            return self.contents[start..];
        }

        return self.contents[start..self.getIndex()];
    }
};

pub fn parseFile(allocator: std.mem.Allocator, filename: []const u8, out_parser: ?*Parser) !*Table {
    var parser = try Parser.initWithFile(allocator, filename);
    if (out_parser) |op| {
        op.* = parser;
        return try op.parse();
    }
    return try parser.parse();
}

pub fn parseContents(allocator: std.mem.Allocator, contents: []const u8, out_parser: ?*Parser) !*Table {

    // TODO: time values
    // TODO: float values
    // TODO: inline tables
    var parser = try Parser.initWithString(allocator, contents);
    if (out_parser) |op| {
        op.* = parser;
        return try op.parse();
    }
    return try parser.parse();
}

test "test.toml file" {
    const filename = "test/test.toml";
    const allocator = std.testing.allocator;

    var parser: Parser = undefined;
    defer parser.deinit();

    var table = try parseFile(allocator, filename, &parser);
    defer table.deinit();
}

test "basic.toml file" {
    const filename = "test/basic.toml";
    const allocator = std.testing.allocator;

    var parser: Parser = undefined;
    defer parser.deinit();

    var table = try parseFile(allocator, filename, &parser);
    defer table.deinit();

    assert(table.keys.get("foo") != null);
    if (table.keys.get("foo")) |foo| {
        assert(foo == .Table);
        assert(foo.Table.keys.get("hi") != null);
        if (foo.Table.keys.get("hi")) |value| {
            assert(std.mem.eql(u8, value.String, "there"));
        }
    }
}

test "comment before newline" {
    var table = try parseContents(std.testing.allocator,
        \\foo="test" # foo
        \\[bar]
        \\
    , null);
    defer table.deinit();

    var foo = table.keys.get("foo").?;
    assert(std.mem.eql(u8, foo.String, "test"));
}

test "whitespace before table" {
    var table = try parseContents(std.testing.allocator,
        \\      [foo.hi]
        \\
        \\        bar    =       1234       # derp
        \\
        \\  [bar]
        \\
        \\    test = true
    , null);
    defer table.deinit();

    var foo = table.keys.get("foo").?;
    var hi = foo.Table.keys.get("hi").?;
    var bar = hi.Table.keys.get("bar").?;
    assert(bar.Integer == 1234);
}

test "multiple key identifiers" {
    var table = try parseContents(std.testing.allocator,
        \\ [foo.bar.foobar]
        \\
    , null);
    defer table.deinit();
}

test "multi-line arrays" {
    var table = try parseContents(std.testing.allocator,
        \\ array = [
        \\ "]",
        \\ # inner comment
        \\ ]
    , null);
    defer table.deinit();
}

test "key value pair" {
    var table = try parseContents(std.testing.allocator,
        \\ foo="hello"
        \\
    , null);
    defer table.deinit();

    assert(table.keys.get("foo") != null);
    if (table.keys.get("foo")) |value| {
        assert(std.mem.eql(u8, value.String, "hello"));
    }
}

test "table" {
    var table = try parseContents(std.testing.allocator,
        \\ [foo]
        \\
    , null);
    defer table.deinit();
    assert(table.keys.get("foo") != null);
}

test "comment" {
    var table = try parseContents(std.testing.allocator,
        \\ # [foo]
        \\
    , null);
    defer table.deinit();
    assert(table.keys.get("foo") == null);
}

test "comment inside array" {
    var table = try parseContents(std.testing.allocator,
        \\ foo=[ # hello there
        \\ 123, 456
        \\ ]
        \\
    , null);
    defer table.deinit();
    var fooKey = table.keys.get("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Array.items.len == 2);
        assert(foo.Array.items[0].Integer == 123);
        assert(foo.Array.items[1].Integer == 456);
    }
}

test "table key value pair" {
    var table = try parseContents(std.testing.allocator,
        \\ [foo]
        \\ key = "bar"
        \\
    , null);
    defer table.deinit();

    assert(table.keys.get("foo") != null);
    if (table.keys.get("foo")) |foo| {
        assert(foo.Table.keys.get("key") != null);
        if (foo.Table.keys.get("key")) |value| {
            assert(std.mem.eql(u8, value.String, "bar"));
        }
    }
}

test "dotted key with string" {
    var table = try parseContents(std.testing.allocator,
        \\ key."ziglang.org" = "bar"
        \\
    , null);
    defer table.deinit();

    assert(table.keys.get("key") != null);
    if (table.keys.get("key")) |value| {
        if (value.Table.keys.get("ziglang.org")) |zig| {
            assert(std.mem.eql(u8, zig.String, "bar"));
        }
    }
}

test "multiple tables" {
    var table = try parseContents(std.testing.allocator,
        \\ [foo]
        \\ key="bar"
        \\ [derp]
        \\ another="foobar"
        \\
    , null);
    defer table.deinit();

    assert(table.keys.get("foo") != null);
    if (table.keys.get("foo")) |foo| {
        assert(foo.Table.keys.get("key") != null);
        if (foo.Table.keys.get("key")) |value| {
            assert(std.mem.eql(u8, value.String, "bar"));
        }
    }

    assert(table.keys.get("derp") != null);
    if (table.keys.get("derp")) |foo| {
        assert(foo.Table.keys.get("another") != null);
        if (foo.Table.keys.get("another")) |value| {
            assert(std.mem.eql(u8, value.String, "foobar"));
        }
    }
}

test "key value pair with string key" {
    var table = try parseContents(std.testing.allocator,
        \\ "foo"="hello"
        \\
    , null);
    defer table.deinit();

    var keyValue = table.keys.get("foo");
    assert(keyValue != null);
    if (keyValue) |value| {
        assert(std.mem.eql(u8, value.String, "hello"));
    }
}

test "dotted key value pair" {
    var table = try parseContents(std.testing.allocator,
        \\ foo.bar="hello"
        \\
    , null);
    defer table.deinit();

    var fooTable = table.keys.get("foo");
    assert(fooTable != null);
    if (fooTable) |foo| {
        var barKey = foo.Table.keys.get("bar");
        assert(barKey != null);
        if (barKey) |value| {
            assert(std.mem.eql(u8, value.String, "hello"));
        }
    }
}

test "dotted key value pair within table" {
    var table = try parseContents(std.testing.allocator,
        \\ [foobar]
        \\ foo.bar="hello"
        \\
    , null);
    defer table.deinit();

    var fooBarTable = table.keys.get("foobar");
    if (fooBarTable) |foobar| {
        var fooTable = foobar.Table.keys.get("foo");
        assert(fooTable != null);
        if (fooTable) |foo| {
            var barKey = foo.Table.keys.get("bar");
            assert(barKey != null);
            if (barKey) |value| {
                assert(std.mem.eql(u8, value.String, "hello"));
            }
        }
    }
}

test "key value pair boolean true" {
    var table = try parseContents(std.testing.allocator,
        \\foo=true
        \\
    , null);
    defer table.deinit();

    var fooKey = table.keys.get("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Boolean == true);
    }
}

test "key value pair boolean false" {
    var table = try parseContents(std.testing.allocator,
        \\ foo=false
        \\
    , null);
    defer table.deinit();

    var fooKey = table.keys.get("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Boolean == false);
    }
}

test "key value pair float 1" {
    var table = try parseContents(std.testing.allocator,
        \\ foo=12.34
        \\
    , null);
    defer table.deinit();

    var fooKey = table.keys.get("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Float == 12.34);
    }
}

test "key value pair float 2" {
    var table = try parseContents(std.testing.allocator,
        \\ foo=.1234
        \\
    , null);
    defer table.deinit();

    var fooKey = table.keys.get("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Float == 0.1234);
    }
}

test "multiple float key value pairs" {
    var table = try parseContents(std.testing.allocator,
        \\ foo=12.34
        \\ bar=43.314
    , null);
    defer table.deinit();

    var fooKey = table.keys.get("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Float == 12.34);
    }

    var barKey = table.keys.get("bar");
    assert(barKey != null);
    if (barKey) |bar| {
        assert(bar.Float == 43.314);
    }
}

test "key value pair integer" {
    var table = try parseContents(std.testing.allocator,
        \\ foo=1234
        \\
    , null);
    defer table.deinit();

    var fooKey = table.keys.get("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Integer == 1234);
    }
}

test "key value pair integer with digit group separator" {
    var table = try parseContents(std.testing.allocator,
        \\ foo=1_1234_3_4
        \\
    , null);
    defer table.deinit();

    var fooKey = table.keys.get("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Integer == 1123434);
    }
}

test "key value pair negative integer" {
    var table = try parseContents(std.testing.allocator,
        \\ foo=-1234
        \\
    , null);
    defer table.deinit();

    var fooKey = table.keys.get("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Integer == -1234);
    }
}

test "key value pair positive integer" {
    var table = try parseContents(std.testing.allocator,
        \\ foo=+1234
        \\
    , null);
    defer table.deinit();

    var fooKey = table.keys.get("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Integer == 1234);
    }
}

test "multiple key value pair" {
    var table = try parseContents(std.testing.allocator,
        \\ foo=1234
        \\ bar=4321
        \\
    , null);
    defer table.deinit();

    var fooKey = table.keys.get("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Integer == 1234);
    }

    var barKey = table.keys.get("bar");
    assert(barKey != null);
    if (barKey) |bar| {
        assert(bar.Integer == 4321);
    }
}

test "key value simple array" {
    var table = try parseContents(std.testing.allocator,
        \\ foo=[]
        \\
    , null);
    defer table.deinit();

    var fooKey = table.keys.get("foo");
    assert(fooKey != null);
    assert(fooKey.? == .Array);
}

test "key value multiple element array" {
    var table = try parseContents(std.testing.allocator,
        \\ foo=[ 1234, 5678, true, false, "hello" ]
        \\
    , null);
    defer table.deinit();

    var fooKey = table.keys.get("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Array.items.len == 5);
        assert(std.mem.eql(u8, foo.Array.items[4].String, "hello"));
        assert(foo.Array.items[3].Boolean == false);
        assert(foo.Array.items[2].Boolean == true);
        assert(foo.Array.items[1].Integer == 5678);
        assert(foo.Array.items[0].Integer == 1234);
    }
}

test "key value array in array" {
    var table = try parseContents(std.testing.allocator,
        \\ foo=[[[[[1234]], 57789, [1234, 578]]]]
        \\
    , null);
    defer table.deinit();

    var fooKey = table.keys.get("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        var items = foo.Array.items;
        assert(foo.Array.items.len == 1);
        var array1 = items[0];
        assert(array1.Array.items.len == 1);
        var array2 = array1.Array.items[0];
        assert(array2.Array.items.len == 3);
        assert(array2.Array.items[1].Integer == 57789);
        var array3 = array2.Array.items[0];
        assert(array3.Array.items.len == 1);
        var array4 = array3.Array.items[0];
        assert(array3.Array.items.len == 1);
        assert(array4.Array.items[0].Integer == 1234);
    }
}

test "key with string first" {
    var table = try parseContents(std.testing.allocator,
        \\ "foo".bar = "foobar"
        \\
    , null);
    defer table.deinit();

    var fooTable = table.keys.get("foo").?;
    var barKey = fooTable.Table.keys.get("bar").?;
    assert(std.mem.eql(u8, barKey.String, "foobar"));
}

test "table with dotted identifier" {
    var table = try parseContents(std.testing.allocator,
        \\ [foo.bar]
        \\ testKey = "hello"
        \\
    , null);
    defer table.deinit();

    var foo = table.keys.get("foo").?;
    var bar = foo.Table.keys.get("bar").?;
    var testKey = bar.Table.keys.get("testKey").?;
    assert(std.mem.eql(u8, testKey.String, "hello"));
}

test "array of tables" {
    // var table = try parseContents(std.testing.allocator,
    //     \\[[foo]]
    //     \\bar = "hi"
    //     \\[[foo]]
    //     \\bar = "test"
    // , null);
    // defer table.deinit();

    // var foo = table.keys.get("foo").?;
    // assert(foo.isManyTables());
    // assert(foo.ManyTables.items.len == 2);

    // var one = foo.ManyTables.items[0];
    // var bar = one.keys.get("bar").?;
    // assert(std.mem.eql(u8, bar.String, "hi"));

    // var two = foo.ManyTables.items[1];
    // var bar2 = two.keys.get("bar").?;
    // assert(std.mem.eql(u8, bar2.String, "test"));
}

test "window line endings" {
    var table = try parseContents(std.testing.allocator, "foo=1234\r\nbar=5789\r\n", null);
    defer table.deinit();

    assert(table.keys.get("foo") != null);
    assert(table.keys.get("bar") != null);
}

test "empty inline table" {
    var table = try parseContents(std.testing.allocator,
        \\foo = {}
    , null);
    defer table.deinit();

    assert(table.keys.get("foo") != null);
    assert(table.keys.get("foo").? == .Table);
}

test "inline table with keys" {
    var table = try parseContents(std.testing.allocator,
        \\foo = { bar = 1234, foobar = "test string" }
    , null);
    defer table.deinit();

    var foo = table.keys.get("foo").?.Table;
    var bar = foo.keys.get("bar").?.Integer;
    assert(bar == 1234);

    var foobar = foo.keys.get("foobar").?.String;
    assert(std.mem.eql(u8, foobar, "test string"));
}

test "inline table with inline table" {
    var table = try parseContents(std.testing.allocator,
        \\foo = { bar = { foobar = "test string" } }
    , null);
    defer table.deinit();

    var foo = table.keys.get("foo").?.Table;
    var bar = foo.keys.get("bar").?.Table;
    var foobar = bar.keys.get("foobar").?.String;
    assert(std.mem.eql(u8, foobar, "test string"));
}
