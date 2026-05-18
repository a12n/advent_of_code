const std = @import("std");

fn sameType(u: u8, v: u8) bool {
    return std.ascii.toLower(u) == std.ascii.toLower(v);
}

fn canReact(u: u8, v: u8) bool {
    return u != v and std.ascii.toLower(u) == std.ascii.toLower(v);
}

const Link = struct {
    back: u16,
    fwd: u16,

    fn reset(links: []Link) void {
        for (0..links.len) |i| {
            links[i].back = if (i > 0) 1 else 0;
            links[i].fwd = if (i < (links.len - 1)) 1 else 0;
        }
    }

    //      d a b A c C a C B A c C c a D A
    // fwd  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0
    // back 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

    //      a A a A b
    // fwd  1 1 1 1 0
    // back 0 1 1 1 1

    //      A a A b a
    // fwd  1 1 1 1 0
    // back 0 1 1 1 1

    //      A b A b a
    // fwd  1 1 1 1 0
    // back 0 1 1 1 1

    fn remove(links: []Link, i: usize) void {
        std.debug.assert(i < links.len);
        links[i - links[i].back].fwd += links[i].fwd;
        links[i + links[i].fwd].back += links[i].back;
        links[i].back = 0;
        links[i].fwd = 0;
    }

    fn skipChar(links: []Link, buf: []const u8, c: u8) usize {
        std.debug.assert(links.len == buf.len);
        var skipped: usize = 0;
        for (0..links.len) |i| {
            if (std.ascii.toLower(buf[i]) == c) {
                remove(links, i);
                skipped += 1;
            }
        }
        return skipped;
    }
};

fn reactUnits(links: []Link, units: []const u8) usize {
    var cur: usize = 0;
    var reacted: usize = 0;

    while (links[cur].fwd != 0) {
        const next = cur + links[cur].fwd;

        if (canReact(units[cur], units[next])) {
            if (links[cur].back > 0) {
                const prev = cur - links[cur].back;

                Link.remove(links, next);
                Link.remove(links, cur);

                cur = prev;
            } else {
                cur = next + links[next].fwd;
                links[cur].back = 0;
            }

            reacted += 2;
        } else {
            cur = next;
        }
    }

    return reacted;
}

fn takePolymer(gpa: std.mem.Allocator, reader: *std.Io.Reader) ![]const u8 {
    var line_accum = std.Io.Writer.Allocating.init(gpa);
    // Sentinel spaces before and after actual polymer units.
    try line_accum.writer.printAsciiChar(' ', .{});
    _ = try reader.streamDelimiter(&line_accum.writer, '\n');
    try line_accum.writer.printAsciiChar(' ', .{});
    return line_accum.written();
}

pub fn part1(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const allocator = init.arena.allocator();

    const polymer = try takePolymer(allocator, stdin);
    const links: []Link = try allocator.alloc(Link, polymer.len);

    Link.reset(links);
    const removed = reactUnits(links, polymer);

    try stdout.print("{d}\n", .{polymer.len - removed - 2});
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const allocator = init.arena.allocator();

    const polymer = try takePolymer(allocator, stdin);
    const links: []Link = try allocator.alloc(Link, polymer.len);

    var removed: usize = undefined;

    Link.reset(links);
    removed = Link.skipChar(links, polymer, 'a');
    removed += reactUnits(links, polymer);
    var max_removed = removed;
    for ('b'..'z' + 1) |c| {
        Link.reset(links);
        removed = Link.skipChar(links, polymer, @intCast(c));
        removed += reactUnits(links, polymer);
        if (removed > max_removed) {
            max_removed = removed;
        }
    }

    try stdout.print("{d}\n", .{polymer.len - max_removed - 2});
}
