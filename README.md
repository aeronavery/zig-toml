[![Build Status](https://github.com/aeronavery/zig-toml/actions/workflows/unit_tests.yml/badge.svg)](https://github.com/aeronavery/zig-toml/actions/workflows/unit_tests.yml)

# zig-toml
A TOML parser written in Zig targeting [TOML v0.5.0](https://github.com/toml-lang/toml). The end goal is for the program to be robust, with no crashing given any situation. Though, in its current state this program is probably far from that goal.

There is currently a lack of documentation but the tests contained should give enough of an idea on how to use this.

[Quickstart Guide](https://github.com/darithorn/zig-toml/wiki/Quickstart-Guide)

## Zig Version Support
This table shows the project's current compatibility status with zig:

| Zig Version | Status |
| -- | -- |
| [0.11.0](https://ziglang.org/documentation/0.11.0/) | ✅ Supported |
| [0.10.1 and below](https://ziglang.org/documentation/0.10.1/) | ❌ Not Supported |

## Features
- [x] Tables
- [x] Keys (all kinds)
- [x] Single-line strings
- [x] Integers
- [x] Booleans
- [x] Arrays
- [x] Comments
- [x] Floats
- [x] Hexadecimal, octal, and binary numbers
- [ ] Multi-line strings
- [ ] String escapes
- [x] Inline tables
- [x] Array of tables
- [ ] Time and date

## Building and Usage

This library is self-contained and requires no dependencies.

To build simply run `zig build` and that will output a file called `libtoml.a` that you can link with your program.

If you want to run the tests then use the `zig build test` command.

## Installation

Add this to you build.zig
```zig
    const zigtoml = b.dependency("zigtoml", .{
        .target = target,
        .optimize = optimize,
    });
    exe.addModule("toml", zigtoml.module("toml"));
```

Add this to your build.zig.zon

```zig
    .zigtoml = .{
       .url = "https://github.com/aeronavery/zig-toml/archive/refs/heads/master.tar.gz",
       //hash will be suggested by the zig compiler
    }
```

This can then be imported into your code like this

```zig
const toml = @import("toml");

```
