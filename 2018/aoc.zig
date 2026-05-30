const std = @import("std");

pub const math = struct {
    pub fn sum(comptime T: type, items: []const T) T {
        var acc: T = std.mem.zeroes(T);
        for (items) |x| {
            acc += x;
        }
        return acc;
    }
};

pub const grid = struct {
    pub const planar = struct {
        pub const Point = @Vector(2, isize);
        pub const Rotation = enum { cw, ccw };
        pub const Vector = @Vector(2, isize);

        pub fn taxicabNorm(u: Vector) usize {
            return @reduce(.Add, @abs(u));
        }

        pub fn taxicabDist(p: Point, q: Point) usize {
            return taxicabNorm(p - q);
        }

        pub fn centroid(points: []Point) ?Point {
            if (points.len == 0) {
                return null;
            }
            return math.sum(Point, points) / @as(Point, @splat(@intCast(points.len)));
        }

        pub fn rotate(rdir: Rotation, u: Vector) Vector {
            return switch (rdir) {
                .cw => .{ -u[1], u[0] },
                .ccw => .{ u[1], -u[0] },
            };
        }

        pub fn isHoriz(u: Vector) bool {
            return u[0] != 0 and u[1] == 0;
        }

        pub fn isVert(u: Vector) bool {
            return u[0] == 0 and u[1] != 0;
        }

        pub const SpiralPointIterator = struct {
            pub const Self = @This();

            pos: Point = .{ 0, 0 },
            dir: Vector = .{ 1, 0 },
            rdir: Rotation = .ccw,

            index: usize = 0,
            len: usize = 1,

            pub fn next(self: *Self) ?Point {
                const pos = self.pos;

                self.index += 1;
                self.pos += self.dir;
                if (self.index == self.len) {
                    if (isVert(self.dir)) {
                        self.len += 1;
                    }
                    self.index = 0;
                    self.dir = rotate(self.rdir, self.dir);
                }

                return pos;
            }
        };

        pub const Extent = struct {
            const Self = @This();

            begin: Point = .{ std.math.maxInt(isize), std.math.maxInt(isize) },
            end: Point = .{ std.math.minInt(isize), std.math.minInt(isize) },

            pub fn size(self: Self) Vector {
                return @max(self.end - self.begin, @as(Vector, @splat(0)));
            }

            pub fn area(self: Self) usize {
                return @intCast(@reduce(.Mul, self.size()));
            }

            pub fn contains(self: Self, p: Point) bool {
                return p[0] >= self.begin[0] and p[0] < self.end[0] and
                    p[1] >= self.begin[1] and p[1] < self.end[1];
            }

            pub fn expand(self: Self, n: isize) Self {
                return .{
                    .begin = self.begin - Vector{ n, n },
                    .end = self.end + Vector{ n, n },
                };
            }

            pub fn initMany(points: []const Point) Self {
                var self = Self{};
                for (points) |p| {
                    self = self.insert(p);
                }
                return self;
            }

            pub fn initOne(p: Point) Self {
                return .{
                    .begin = p,
                    .end = p + Vector{ 1, 1 },
                };
            }

            pub fn insert(self: Self, p: Point) Self {
                return .{
                    .begin = @min(self.begin, p),
                    .end = @max(self.end, p + Vector{ 1, 1 }),
                };
            }

            pub fn iterate(self: Self) Iterator {
                return .{
                    .extent = self,
                    .pos = self.begin,
                };
            }

            const Iterator = struct {
                extent: Extent,
                pos: Point,

                pub fn next(self: *Iterator) ?Point {
                    const pos = self.pos;

                    if (!self.extent.contains(pos)) {
                        return null;
                    }

                    self.pos[0] += 1;
                    if (self.pos[0] >= self.extent.end[0]) {
                        self.pos[0] = self.extent.begin[0];
                        self.pos[1] += 1;
                    }

                    return pos;
                }
            };
        };
    };
};

test "rotate vector" {
    const expectEqual = std.testing.expectEqual;

    const Vector = grid.planar.Vector;
    const rotate = grid.planar.rotate;

    try expectEqual(Vector{ 0, 0 }, rotate(.cw, Vector{ 0, 0 }));
    try expectEqual(Vector{ 0, 1 }, rotate(.cw, Vector{ 1, 0 }));
    try expectEqual(Vector{ 2, 0 }, rotate(.cw, Vector{ 0, -2 }));
    try expectEqual(Vector{ 0, -3 }, rotate(.cw, Vector{ -3, 0 }));
    try expectEqual(Vector{ -4, 0 }, rotate(.cw, Vector{ 0, 4 }));

    try expectEqual(Vector{ 0, 0 }, rotate(.ccw, Vector{ 0, 0 }));
    try expectEqual(Vector{ 0, -1 }, rotate(.ccw, Vector{ 1, 0 }));
    try expectEqual(Vector{ -2, 0 }, rotate(.ccw, Vector{ 0, -2 }));
    try expectEqual(Vector{ 0, 3 }, rotate(.ccw, Vector{ -3, 0 }));
    try expectEqual(Vector{ 4, 0 }, rotate(.ccw, Vector{ 0, 4 }));
}

test "spiral point iterator" {
    const expectEqual = std.testing.expectEqual;

    const Point = grid.planar.Point;
    const SpiralPointIterator = grid.planar.SpiralPointIterator;

    const x: isize = 11;
    const y: isize = 17;
    var iter: SpiralPointIterator = .{ .pos = Point{ x, y } };
    try expectEqual(Point{ x + 0, y + 0 }, iter.next().?);
    try expectEqual(Point{ x + 1, y + 0 }, iter.next().?);
    try expectEqual(Point{ x + 1, y - 1 }, iter.next().?);
    try expectEqual(Point{ x + 0, y - 1 }, iter.next().?);
    try expectEqual(Point{ x - 1, y - 1 }, iter.next().?);
    try expectEqual(Point{ x - 1, y + 0 }, iter.next().?);
    try expectEqual(Point{ x - 1, y + 1 }, iter.next().?);
    try expectEqual(Point{ x + 0, y + 1 }, iter.next().?);
    try expectEqual(Point{ x + 1, y + 1 }, iter.next().?);
    try expectEqual(Point{ x + 2, y + 1 }, iter.next().?);
    try expectEqual(Point{ x + 2, y + 0 }, iter.next().?);
    try expectEqual(Point{ x + 2, y - 1 }, iter.next().?);
    try expectEqual(Point{ x + 2, y - 2 }, iter.next().?);
    try expectEqual(Point{ x + 1, y - 2 }, iter.next().?);
    try expectEqual(Point{ x + 0, y - 2 }, iter.next().?);
    try expectEqual(Point{ x - 1, y - 2 }, iter.next().?);
    try expectEqual(Point{ x - 2, y - 2 }, iter.next().?);
    try expectEqual(Point{ x - 2, y - 1 }, iter.next().?);
}

pub fn run(
    init: std.process.Init,
    func: *const fn (std.process.Init, *std.Io.Reader, *std.Io.Writer, u1) anyerror!void,
) !void {
    var args_iter = init.minimal.args.iterate();
    const stem = std.fs.path.stem(args_iter.next() orelse return error.InvalidExecName);
    if (stem.len != "00-0".len) {
        return error.InvalidExecName;
    }

    const day = std.fmt.parseUnsigned(u8, stem[0..2], 10) catch return error.InvalidPuzzle;
    const part = std.fmt.parseUnsigned(u8, stem[3..4], 10) catch return error.InvalidPuzzle;
    if (day < 1 or day > 25 or part < 1 or (day == 25 and part > 1) or part > 2) {
        return error.InvalidPuzzle;
    }

    var stdin_buf: [3584]u8 = undefined;
    var stdin_reader = std.Io.File.stdin().reader(init.io, &stdin_buf);
    const stdin = &stdin_reader.interface;

    var stdout_buf: [512]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(init.io, &stdout_buf);
    const stdout = &stdout_writer.interface;

    try func(init, stdin, stdout, @intCast(part - 1));
}
