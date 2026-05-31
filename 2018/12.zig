const std = @import("std");

const Plant = u1;
const Dictionary = [2][2][2][2][2]Plant;

fn charToPlant(c: u8) !Plant {
    return switch (c) {
        '.' => 0,
        '#' => 1,
        else => error.InvalidInput,
    };
}

fn plantToChar(plant: Plant) u8 {
    return switch (plant) {
        0 => '.',
        1 => '#',
    };
}

const PotsArray = struct {
    const zero_offset = 50;
    const min_pot = -zero_offset;
    const max_pot = 2 * zero_offset;

    // Negative pots, zero, positive pots.
    items: [max_pot - min_pot + 1]Plant = .{0} ** (max_pot - min_pot + 1),

    fn index(pot: isize) usize {
        return @intCast(pot + zero_offset);
    }

    fn unindex(i: usize) isize {
        return @as(isize, @intCast(i)) - zero_offset;
    }

    fn get(self: PotsArray, pot: isize) Plant {
        return if (pot < min_pot or pot > max_pot) 0 else self.items[index(pot)];
    }

    fn set(self: *PotsArray, pot: isize, plant: Plant) void {
        self.items[index(pot)] = plant;
    }

    fn debugPrint(self: PotsArray, gen: usize) !void {
        std.debug.print("{d:02}: ", .{gen});
        for (self.items) |plant| {
            std.debug.print("{c}", .{plantToChar(plant)});
        }
        std.debug.print("\n", .{});
    }

    fn sumPlants(self: PotsArray) isize {
        var sum: isize = 0;
        for (0.., self.items) |i, plant| {
            if (plant != 0) {
                sum += unindex(i);
            }
        }
        return sum;
    }
};

fn readInput(reader: *std.Io.Reader) !struct { Dictionary, PotsArray } {
    var pots: PotsArray = .{};

    if (try reader.takeDelimiter('\n')) |line| {
        var fields = std.mem.tokenizeAny(u8, line, "initial state: ");
        const init_str = fields.next() orelse return error.InvalidInput;
        if (fields.next() != null) return error.InvalidInput;

        for (0.., init_str) |pot, c| {
            pots.set(@intCast(pot), try charToPlant(c));
        }
    } else {
        return error.InvalidInput;
    }

    if (try reader.takeDelimiter('\n')) |line| {
        if (line.len != 0) {
            return error.InvalidInput;
        }
    } else {
        return error.InvalidInput;
    }

    var dict: Dictionary = std.mem.zeroes(Dictionary);

    while (try reader.takeDelimiter('\n')) |line| {
        var fields = std.mem.tokenizeAny(u8, line, " => ");
        const pattern_str = fields.next() orelse return error.InvalidInput;
        const plant_str = fields.next() orelse return error.InvalidInput;
        if (fields.next() != null) return error.InvalidInput;

        if (pattern_str.len != 5) return error.InvalidInput;
        const pattern: [5]Plant = .{
            try charToPlant(pattern_str[0]),
            try charToPlant(pattern_str[1]),
            try charToPlant(pattern_str[2]),
            try charToPlant(pattern_str[3]),
            try charToPlant(pattern_str[4]),
        };

        if (plant_str.len != 1) return error.InvalidInput;
        const plant: Plant = try charToPlant(plant_str[0]);

        dict[pattern[0]][pattern[1]][pattern[2]][pattern[3]][pattern[4]] = plant;

        std.debug.print("pattern {any} plant {any}\n", .{ pattern, plant });
    }

    return .{ dict, pots };
}

// The empty bit pattern can't map to 1. Otherwise it will require to
// processs the infinite string of bits:
// ..... => .
//
// Then the first pattern that could match at the beginning of the string is
// ....#
// So, the initial string has to be extended to the left by 4 zero
// bits. The first pattern match may set a bit two bits before the
// beginning of the string.
//
// The same applies to the end of the string: the last part of the
// string could match the "#...." pattern and may set a bit two bits past
// the end of the string.
pub fn part1(_: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const dict, var pots = try readInput(stdin);

    var first_pot = PotsArray.unindex(std.mem.findScalar(Plant, &pots.items, 1) orelse return error.InvalidInput);
    var last_pot = PotsArray.unindex(std.mem.findScalarLast(Plant, &pots.items, 1) orelse return error.InvalidInput);

    std.debug.print("first_pot {d} last_pot {d}\n", .{ first_pot, last_pot });

    try pots.debugPrint(0);
    for (0..20) |gen| {
        try pots.debugPrint(gen + 1);

        var next = pots;
        var pot = first_pot - 2;
        while (pot <= (last_pot + 2)) : (pot += 1) {
            const next_plant = dict[pots.get(pot - 2)][pots.get(pot - 1)][pots.get(pot)][pots.get(pot + 1)][pots.get(pot + 2)];
            next.set(pot, next_plant);
        }
        pots = next;
        first_pot -= 2;
        last_pot += 2;
    }

    try stdout.print("{d}\n", .{pots.sumPlants()});
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    _ = init;
    _ = stdin;
    _ = stdout;
    // TODO
}
