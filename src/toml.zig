const std = @import("std");
const assert = std.debug.assert;

const Key = union(enum) {
    None,
    DottedIdent: DottedIdentifier,
    Ident: []const u8,
};

pub const DynamicArray = std.TailQueue(Value);

/// utility function helping to index the DynamicArray
pub fn indexArray(array: DynamicArray, index: usize) ?Value {
    var i = index;
    var it = array.first;
    while (it) |node| : (it = node.next) {
        if (i == 0) {
            return node.data;
        }
        i -= 1;
    }
    return null;
}

pub const Value = union(enum) {
    None,
    String: []const u8,
    Boolean: bool,
    Integer: i64,
    Array: DynamicArray,
};

pub const Table = struct {
    const Self = @This();
    const TableMap = std.AutoHashMap([]const u8, *Table);
    const KeyMap = std.AutoHashMap([]const u8, Value);

    pub children: TableMap,
    pub keys: KeyMap,
    pub name: []const u8,

    allocator: *std.mem.Allocator,

    pub fn init(allocator: *std.mem.Allocator, name: []const u8) Self {
        return Self{
            .children = TableMap.init(allocator),
            .keys = TableMap.init(allocator),
            .name = name,
            .allocator = allocator,
        };
    }

    pub fn create(allocator: *std.mem.Allocator, name: []const u8) !*Self {
        var result = try allocator.create(Table);
        result.* = Table.init(allocator, name);
        return result;
    }

    /// Cleans up the table's keys and its children
    pub fn deinit(self: *Self) void {
        // TODO: implement deinit
    }

    pub fn addKey(self: *Self, key: Key, value: Value) !void {
        // TODO: test for key clobbering and give an error
        switch (key) {
            Key.None => {
                return;
            },
            Key.Ident => |name| {
                _ = try self.keys.put(name, value);
            },
            Key.DottedIdent => |dotted| {
                var currentTable: *Table = self;
                var index: usize = 0;
                while (index < dotted.len - 1) : (index += 1) {
                    if (currentTable.children.getValue(indexIdentifier(dotted, index).?)) |table| {
                        currentTable = table;
                    } else {
                        var table = try self.allocator.create(Table);
                        table.* = Table.init(self.allocator, indexIdentifier(dotted, index).?);
                        try currentTable.addTable(table);
                        currentTable = table;
                    }
                }
                _ = try currentTable.keys.put(indexIdentifier(dotted, index).?, value);
            },
        }
    }

    pub fn getKey(self: Self, key: []const u8) ?Value {
        if (self.keys.getValue(key)) |value| {
            return value;
        } else {
            var it = self.children.iterator();
            while (it.next()) |nextChild| {
                // recursion is unavoidable here
                if (nextChild.value.getKey(key)) |value| {
                    return value;
                }
            }
        }
        return null;
    }

    pub fn getTable(self: Self, name: []const u8) ?Table {
        if (self.children.getValue(name)) |table| {
            return table.*;
        } else {
            var it = self.children.iterator();
            while (it.next()) |nextChild| {
                // the recursion is unavoidable here
                if (nextChild.value.getTable(name)) |table| {
                    return table;
                }
            }
        }
        return null;
    }

    pub fn addTable(self: *Self, table: *Table) !void {
        _ = try self.children.put(table.name, table);
    }

    pub fn addNewTable(self: *Self, name: []const u8) !*Table {
        var table = try Table.create(self.allocator, name);
        _ = try self.children.put(name, table);
        return table;
    }
};

const State = enum {
    None,
    EOF,
    Identifier,
    Table,
    String,
    Number,
    Comment,
};

const ParseError = error{
    UnexpectedEOF,
    ExpectedNewline,
    MalformedString,
    MalformedKey,
    MalformedTable,
    ExpectedEquals,
    ExpectedComma,
    ExpectedCloseArray,
    InvalidValue,
};

fn isIdentifier(c: u8) bool {
    return (c >= 65 and c <= 90) or (c >= 48 and c <= 57) or (c >= 97 and c <= 122) or c == '-' or c == '_';
}

fn isQuote(c: u8) bool {
    return c == '"' or c == '\'';
}

fn isWhitespace(c: u8) bool {
    return c == '\n' or c == '\t' or c == ' ' or c == '\r';
}

fn isNumber(word: []const u8) bool {
    var i: usize = 0;
    if (word[i] == '_') {
        return false;
    }
    if (word[i] == '-' or word[i] == '+') {
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
        }
        if (!(c >= 48 and c <= 57)) {
            return false;
        }
    }
    return true;
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

/// function to easily get the next non-whitespace character
fn getNextChar(contents: []const u8, start: usize) usize {
    if (start >= contents.len) {
        return start;
    }
    var index = start + 1;
    while (index < contents.len and isWhitespace(contents[index])) {
        index += 1;
    }

    if (index >= contents.len) {
        return index;
    }

    return index;
}

fn getState(contents: []const u8, index: usize) State {
    if (index >= contents.len) {
        return State.EOF;
    }
    var c = contents[index];
    if (isIdentifier(c)) {
        return State.Identifier;
    }
    switch (c) {
        '[' => {
            return State.Table;
        },
        '"' => {
            return State.String;
        },
        '-', '+' => {
            return State.Number;
        },
        '#' => {
            return State.Comment;
        },
        else => {
            return State.None;
        },
    }
}

const DottedIdentifier = std.TailQueue([]const u8);

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

/// This assumes that the starting character aka contents[start] is a '.'
fn parseDottedIdentifier(allocator: *std.mem.Allocator, contents: []const u8, start: *usize) !DottedIdentifier {
    var result = DottedIdentifier.init();
    if (start.* >= contents.len) {
        return ParseError.UnexpectedEOF;
    }

    while (contents[start.*] == '.') {
        var word: []const u8 = undefined;
        start.* += 1;
        if (isQuote(contents[start.*])) {
            word = try parseString(contents, start);
        } else {
            word = try parseIdentifier(contents, start);
        }
        start.* += 1;
        var node = try result.createNode(word, allocator);
        result.append(node);
        if (start.* >= contents.len) {
            break;
        }
    }

    start.* -= 1;
    return result;
}

/// Assumes the starting character is a " or a '
fn parseString(contents: []const u8, index: *usize) ParseError![]const u8 {
    var start = index.*;
    var i = index.*;
    if (i >= contents.len) {
        return ParseError.UnexpectedEOF;
    }
    // TODO: multiline strings
    var quote = contents[i];

    if (i + 1 >= contents.len) {
        return ParseError.MalformedString;
    }
    i += 1;
    var c = contents[i];
    // TODO: escaped characters
    while (c != quote) {
        i += 1;
        if (i >= contents.len) {
            return ParseError.MalformedString;
        }
        c = contents[i];
    }

    index.* = i;
    return contents[start + 1 .. i];
}

fn parseIdentifier(contents: []const u8, index: *usize) ![]const u8 {
    var start = index.*;

    var i = start;
    if (i >= contents.len) {
        return ParseError.UnexpectedEOF;
    }
    var c = contents[i];
    while (isIdentifier(c)) {
        i += 1;
        if (i >= contents.len) {
            return ParseError.UnexpectedEOF;
        }
        c = contents[i];
    }

    index.* = i - 1;
    return contents[start .. index.* + 1];
}

fn convertIdentifierToValue(word: []const u8) !Value {
    if (std.mem.eql(u8, word, "true")) {
        return Value{ .Boolean = true };
    } else if (std.mem.eql(u8, word, "false")) {
        return Value{ .Boolean = false };
    } else if (isNumber(word)) {
        return Value{ .Integer = toInteger(word) };
    } else {
        return ParseError.InvalidValue;
    }
}

fn parseArray(allocator: *std.mem.Allocator, contents: []const u8, index: *usize) anyerror!Value {
    var array = DynamicArray.init();

    var expectClosing = false;
    var i = index.*;
    var state = getState(contents, i);
    while (i < contents.len) {
        if (expectClosing and state != State.None) {
            return ParseError.ExpectedComma;
        }
        switch (state) {
            State.None => {
                if (i < contents.len) {
                    if (contents[i] == ']') {
                        index.* = i;
                        return Value{ .Array = array };
                    }
                }
                i += 1;
                state = getState(contents, i);
                continue;
            },
            State.EOF => {
                break;
            },
            State.Comment => {
                i = skipComment(contents, i);
                i += 1;
                state = getState(contents, i);
                continue;
            },
            State.Identifier => {
                var word = try parseIdentifier(contents, &i);
                var value = try convertIdentifierToValue(word);
                var node = try array.createNode(value, allocator);
                array.append(node);
            },
            State.String => {
                var str = try parseString(contents, &i);
                var value = Value{ .String = str };
                var node = try array.createNode(value, allocator);
                array.append(node);
            },
            State.Number => {
                if (contents[i] == '+') {
                    i += 1;
                }
                var word = try parseIdentifier(contents, &i);
                if (isNumber(word)) {
                    var value = Value{ .Integer = toInteger(word) };
                    var node = try array.createNode(value, allocator);
                    array.append(node);
                } else {
                    return ParseError.InvalidValue;
                }
            },
            State.Table => {
                i += 1;
                var value = try parseArray(allocator, contents, &i);
                var node = try array.createNode(value, allocator);
                array.append(node);
            },
        }

        i += 1;
        if (i < contents.len) {
            if (contents[i] != ',') {
                expectClosing = true;
            }
        }

        state = getState(contents, i);
    }

    return ParseError.ExpectedCloseArray;
}

fn skipComment(contents: []const u8, start: usize) usize {
    var i = start;
    while (i < contents.len) {
        if (contents[i] == '\n') {
            return i - 1;
        }
        i += 1;
    }
    return i;
}

fn parseKeyIdentifier(allocator: *std.mem.Allocator, contents: []const u8, index: *usize) !Key {
    var word: []const u8 = undefined;
    if (isQuote(contents[index.*])) {
        word = try parseString(contents, index);
    } else {
        word = try parseIdentifier(contents, index);
    }
    if (index.* + 1 < contents.len and contents[index.* + 1] == '.') {
        index.* += 1;
        var dottedResult = try parseDottedIdentifier(allocator, contents, index);
        var node = try dottedResult.createNode(word, allocator);
        dottedResult.prepend(node);
        return Key{ .DottedIdent = dottedResult };
    } else {
        return Key{ .Ident = word };
    }
}

fn parseTable(allocator: *std.mem.Allocator, name: []const u8, contents: []const u8, index: *usize) anyerror!*Table {
    var table = try Table.create(allocator, name);
    var i = index.*;
    var key: Key = Key.None;
    var value: Value = Value.None;
    var state = getState(contents, index.*);
    while (i < contents.len) {
        switch (state) {
            State.None => {},
            State.EOF => {
                break;
            },
            State.Comment => {
                i = skipComment(contents, i);
            },
            State.Identifier => {
                // boolean value identifier
                if (key != Key.None and value == Value.None) {
                    var word = try parseIdentifier(contents, &i);
                    value = try convertIdentifierToValue(word);
                } else {
                    var keyIdentifier = try parseKeyIdentifier(allocator, contents, &i);
                    // regular identifier
                    if (key == Key.None) {
                        key = keyIdentifier;
                    } else {
                        return ParseError.MalformedKey;
                    }
                }
            },
            State.Table => {
                // Deals with tables or arrays depending on if key == Key.None
                i += 1;
                if (key != Key.None and value == Value.None) {
                    // treat this as an array
                    value = try parseArray(allocator, contents, &i);
                } else {
                    var tableKey = try parseKeyIdentifier(allocator, contents, &i);
                    var tableName: []const u8 = undefined;
                    var currentTable: *Table = table;
                    switch (tableKey) {
                        Key.None => {
                            return ParseError.MalformedTable;
                        },
                        Key.Ident => |ident| {
                            tableName = ident;
                        },
                        Key.DottedIdent => |dotted| {
                            var it = dotted.first;
                            while (it) |node| : (it = node.next) {
                                if (node == dotted.last) {
                                    break;
                                }
                                currentTable = try table.addNewTable(node.data);
                            }
                            tableName = dotted.last.?.data;
                        },
                    }
                    i += 1;
                    if (contents[i] != ']') {
                        return ParseError.MalformedTable;
                    }
                    var newTable = try parseTable(allocator, tableName, contents, &i);
                    try currentTable.addTable(newTable);
                }
            },
            State.Number => {
                // Deals with numbers starting with +
                if (contents[i] == '+') {
                    i += 1;
                }
                var word = try parseIdentifier(contents, &i);
                if (isNumber(word)) {
                    value = Value{ .Integer = toInteger(word) };
                } else {
                    return ParseError.InvalidValue;
                }
            },
            State.String => {
                if (key == Key.None) {
                    var strKey = try parseKeyIdentifier(allocator, contents, &i);
                    key = strKey;
                } else if (value == Value.None) {
                    var str = try parseString(contents, &i);
                    value = Value{ .String = str };
                }
            },
        }
        // if we have a key and a value pair
        if (value != Value.None and key != Key.None) {
            // add the key value pair to the current table
            try table.addKey(key, value);
            value = Value.None;
            key = Key.None;

            var peek = getNextChar(contents, i);
            if (peek < contents.len and contents[peek] == '#') {
                i = skipComment(contents, peek);
            }

            // TODO: support Windows line endings \r\n
            // enforce a new line after a key-value pair
            i += 1;
            if (i < contents.len and contents[i] != '\n') {
                return ParseError.ExpectedNewline;
            }
        }

        i = getNextChar(contents, i);

        // if we have a key but not a value then the next (non-whitespace) character must be an =
        if (value == Value.None and key != Key.None) {
            if (contents[i] == '=') {
                i = getNextChar(contents, i);
            } else {
                return ParseError.ExpectedEquals;
            }
        }

        state = getState(contents, i);
    }

    index.* = i;
    return table;
}

pub fn parseFile(allocator: *std.mem.Allocator, filename: []const u8) !*Table {
    var contents = try std.io.readFileAlloc(allocator, filename);
    return parseContents(allocator, contents);
}

pub fn parseContents(allocator: *std.mem.Allocator, contents: []const u8) !*Table {
    var index: usize = 0;

    // TODO: time values
    // TODO: float values
    // TODO: inline tables
    // TODO: array of tables

    var globalTable = try parseTable(allocator, "", contents, &index);

    return globalTable;
}

test "test.toml file" {
    var table = try parseFile(std.heap.c_allocator, "test/test.toml");
}

test "key value pair" {
    var table = try parseContents(std.heap.c_allocator,
        \\ foo="hello"
        \\
    );

    assert(table.getKey("foo") != null);
    if (table.getKey("foo")) |value| {
        assert(std.mem.eql(u8, value.String, "hello"));
    }
}

test "table" {
    var table = try parseContents(std.heap.c_allocator,
        \\ [foo]
        \\
    );
    assert(table.getTable("foo") != null);
}

test "comment" {
    var table = try parseContents(std.heap.c_allocator,
        \\ # [foo]
        \\
    );
    assert(table.getTable("foo") == null);
}

test "comment inside array" {
    var table = try parseContents(std.heap.c_allocator,
        \\ foo=[ # hello there
        \\ 123, 456
        \\ ]
        \\
    );
    var fooKey = table.getKey("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Array.len == 2);
    }
}

test "table key value pair" {
    var table = try parseContents(std.heap.c_allocator,
        \\ [foo]
        \\ key = "bar"
        \\
    );

    assert(table.getTable("foo") != null);
    if (table.getTable("foo")) |foo| {
        assert(foo.getKey("key") != null);
        if (foo.getKey("key")) |value| {
            assert(std.mem.eql(u8, value.String, "bar"));
        }
    }
}

test "dotted key with string" {
    var table = try parseContents(std.heap.c_allocator,
        \\ key."ziglang.org" = "bar"
        \\
    );

    assert(table.getTable("key") != null);
    if (table.getTable("key")) |value| {
        if (value.getKey("ziglang.org")) |zig| {
            assert(std.mem.eql(u8, zig.String, "bar"));
        }
    }
}

test "multiple tables" {
    var table = try parseContents(std.heap.c_allocator,
        \\ [foo]
        \\ key="bar"
        \\ [derp]
        \\ another="foobar"
        \\
    );

    assert(table.getTable("foo") != null);
    if (table.getTable("foo")) |foo| {
        assert(foo.getKey("key") != null);
        if (foo.getKey("key")) |value| {
            assert(std.mem.eql(u8, value.String, "bar"));
        }
    }

    assert(table.getTable("derp") != null);
    if (table.getTable("derp")) |foo| {
        assert(foo.getKey("another") != null);
        if (foo.getKey("another")) |value| {
            assert(std.mem.eql(u8, value.String, "foobar"));
        }
    }
}

test "key value pair with string key" {
    var table = try parseContents(std.heap.c_allocator,
        \\ "foo"="hello"
        \\
    );

    var keyValue = table.getKey("foo");
    assert(keyValue != null);
    if (keyValue) |value| {
        assert(std.mem.eql(u8, value.String, "hello"));
    }
}

test "dotted key value pair" {
    var table = try parseContents(std.heap.c_allocator,
        \\ foo.bar="hello"
        \\
    );

    var fooTable = table.getTable("foo");
    assert(fooTable != null);
    if (fooTable) |foo| {
        var barKey = foo.getKey("bar");
        assert(barKey != null);
        if (barKey) |value| {
            assert(std.mem.eql(u8, value.String, "hello"));
        }
    }
}

test "dotted key value pair within table" {
    var table = try parseContents(std.heap.c_allocator,
        \\ [foobar]
        \\ foo.bar="hello"
        \\
    );

    var fooBarTable = table.getTable("foobar");
    if (fooBarTable) |foobar| {
        var fooTable = foobar.getTable("foo");
        assert(fooTable != null);
        if (fooTable) |foo| {
            var barKey = foo.getKey("bar");
            assert(barKey != null);
            if (barKey) |value| {
                assert(std.mem.eql(u8, value.String, "hello"));
            }
        }
    }
}

test "key value pair boolean true" {
    var table = try parseContents(std.heap.c_allocator,
        \\ foo=true
        \\
    );

    var fooKey = table.getKey("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Boolean == true);
    }
}

test "key value pair boolean false" {
    var table = try parseContents(std.heap.c_allocator,
        \\ foo=false
        \\
    );

    var fooKey = table.getKey("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Boolean == false);
    }
}

test "key value pair integer" {
    var table = try parseContents(std.heap.c_allocator,
        \\ foo=1234
        \\
    );

    var fooKey = table.getKey("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Integer == 1234);
    }
}

test "key value pair integer" {
    var table = try parseContents(std.heap.c_allocator,
        \\ foo=1_1234_3_4
        \\
    );

    var fooKey = table.getKey("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Integer == 1123434);
    }
}

test "key value pair negative integer" {
    var table = try parseContents(std.heap.c_allocator,
        \\ foo=-1234
        \\
    );

    var fooKey = table.getKey("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Integer == -1234);
    }
}

test "key value pair positive integer" {
    var table = try parseContents(std.heap.c_allocator,
        \\ foo=+1234
        \\
    );

    var fooKey = table.getKey("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Integer == 1234);
    }
}

test "multiple key value pair" {
    var table = try parseContents(std.heap.c_allocator,
        \\ foo=1234
        \\ bar=4321
        \\
    );

    var fooKey = table.getKey("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Integer == 1234);
    }

    var barKey = table.getKey("bar");
    assert(barKey != null);
    if (barKey) |bar| {
        assert(bar.Integer == 4321);
    }
}

test "key value simple array" {
    var table = try parseContents(std.heap.c_allocator,
        \\ foo=[]
        \\
    );

    var fooKey = table.getKey("foo");
    assert(fooKey != null);
}

test "key value multiple element array" {
    var table = try parseContents(std.heap.c_allocator,
        \\ foo=[ 1234, 5678, true, false, "hello" ]
        \\
    );

    var fooKey = table.getKey("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Array.len == 5);
        assert(std.mem.eql(u8, indexArray(foo.Array, 4).?.String, "hello"));
        assert(indexArray(foo.Array, 3).?.Boolean == false);
        assert(indexArray(foo.Array, 2).?.Boolean == true);
        assert(indexArray(foo.Array, 1).?.Integer == 5678);
        assert(indexArray(foo.Array, 0).?.Integer == 1234);
    }
}

test "key value array in array" {
    var table = try parseContents(std.heap.c_allocator,
        \\ foo=[[[[[1234]], 57789, [1234, 578]]]]
        \\
    );

    var fooKey = table.getKey("foo");
    assert(fooKey != null);
    if (fooKey) |foo| {
        assert(foo.Array.len == 1);
        var array1 = indexArray(foo.Array, 0).?;
        assert(array1.Array.len == 1);
        var array2 = indexArray(array1.Array, 0).?;
        assert(array2.Array.len == 3);
        assert(indexArray(array2.Array, 1).?.Integer == 57789);
        var array3 = indexArray(array2.Array, 0).?;
        assert(array3.Array.len == 1);
        var array4 = indexArray(array3.Array, 0).?;
        assert(array3.Array.len == 1);
        assert(indexArray(array4.Array, 0).?.Integer == 1234);
    }
}

test "key with string first" {
    var table = try parseContents(std.heap.c_allocator,
        \\ "foo".bar = "foobar"
        \\
    );

    var fooTable = table.getTable("foo");
    assert(fooTable != null);
    var barKey = fooTable.?.getKey("bar");
    assert(barKey != null);
    assert(std.mem.eql(u8, barKey.?.String, "foobar"));
}

test "table with dotted identifier" {
    var table = try parseContents(std.heap.c_allocator,
        \\ [foo.bar]
        \\ testKey = "hello"
        \\
    );

    var fooTable = table.getTable("foo");
    assert(fooTable != null);
    if (fooTable) |foo| {
        var barTable = foo.getTable("bar");
        assert(barTable != null);
        if (barTable) |bar| {
            var testKey = bar.getKey("testKey");
            assert(testKey != null);
            assert(std.mem.eql(u8, testKey.?.String, "hello"));
        }
    }
}
