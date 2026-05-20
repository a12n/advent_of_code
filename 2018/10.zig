const std = @import("std");
const aoc = @import("aoc.zig");

const grid = aoc.grid.planar;

const Light = struct { grid.Point, grid.Vector };
const LightArray = std.ArrayList(Light);

fn lightPosition(l: Light, t: isize) grid.Point {
    return l[0] + l[1] * @as(grid.Vector, @splat(t));
}

fn parseCoord(str: []const u8) !isize {
    return try std.fmt.parseInt(isize, str, 10);
}

fn parseLight(str: []const u8) !Light {
    var tokens = std.mem.tokenizeAny(u8, str, "position=< , > velocity=< , >");
    const px = try parseCoord(tokens.next() orelse return error.InvalidInput);
    const py = try parseCoord(tokens.next() orelse return error.InvalidInput);
    const vx = try parseCoord(tokens.next() orelse return error.InvalidInput);
    const vy = try parseCoord(tokens.next() orelse return error.InvalidInput);
    if (tokens.next() != null) return error.InvalidInput;
    return .{ .{ px, py }, .{ vx, vy } };
}

fn readLightArray(gpa: std.mem.Allocator, reader: *std.Io.Reader) !LightArray {
    var lights: LightArray = try .initCapacity(gpa, 400);
    while (try reader.takeDelimiter('\n')) |line| {
        try lights.append(gpa, try parseLight(line));
    }
    return lights;
}

fn waitMessage(lights: []const Light) ?struct { isize, grid.Extent } {
    var t: isize = 0;
    var best_t: ?isize = null;
    var best_extent: ?grid.Extent = null;

    while (t < std.math.maxInt(isize)) : (t += 1) {
        var extent: grid.Extent = .{};

        for (lights) |l| {
            extent = extent.insert(lightPosition(l, t));
        }

        if (best_extent == null or extent.area() < best_extent.?.area()) {
            best_t = t;
            best_extent = extent;
        } else {
            return .{ best_t.?, best_extent.? };
        }
    }

    return null;
}

pub fn part1(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const allocator = init.arena.allocator();
    const lights = try readLightArray(allocator, stdin);
    const t, const extent = waitMessage(lights.items) orelse return error.NoSolution;

    const size: @Vector(2, usize) = @intCast(extent.size());
    var render = try allocator.alloc(u8, size[1] * (size[0] + 1));
    @memset(render, ' ');

    for (lights.items) |l| {
        const offset: @Vector(2, usize) = @intCast(lightPosition(l, t) - extent.begin);
        render[offset[1] * (size[0] + 1) + offset[0]] = '#';
    }

    for (0..size[1]) |y| {
        render[y * (size[0] + 1) + size[0]] = '\n';
    }

    try stdout.printAscii(render, .{});
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const allocator = init.arena.allocator();
    const lights = try readLightArray(allocator, stdin);
    const t, _ = waitMessage(lights.items) orelse return error.NoSolution;
    try stdout.print("{d}\n", .{t});
}
