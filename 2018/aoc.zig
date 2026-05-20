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
