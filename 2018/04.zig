const std = @import("std");

const Timestamp = struct {
    std.time.epoch.Year,
    std.time.epoch.MonthAndDay,
    std.time.epoch.DaySeconds,
};

const Event = union(enum) {
    begins_shift: u16,
    falls_asleep: void,
    wakes_up: void,
};

const TimedEvent = struct {
    Timestamp,
    Event,
};

fn parseTimestamp(buf: []const u8) !Timestamp {
    var fields = std.mem.tokenizeAny(u8, buf, " -:");

    const year_buf = fields.next() orelse return error.InvalidInput;
    const month_buf = fields.next() orelse return error.InvalidInput;
    const day_buf = fields.next() orelse return error.InvalidInput;
    const hour_buf = fields.next() orelse return error.InvalidInput;
    const minute_buf = fields.next() orelse return error.InvalidInput;
    if (fields.next() != null) {
        return error.InvalidInput;
    }

    const year = try std.fmt.parseUnsigned(std.time.epoch.Year, year_buf, 10);
    const month = @as(std.time.epoch.Month, @enumFromInt(try std.fmt.parseUnsigned(u8, month_buf, 10)));
    const day = try std.fmt.parseUnsigned(u5, day_buf, 10);
    const hour = try std.fmt.parseUnsigned(u17, hour_buf, 10);
    const minute = try std.fmt.parseUnsigned(u17, minute_buf, 10);

    // TODO: Validate?
    return .{
        year,
        .{ .month = month, .day_index = day },
        .{ .secs = std.time.s_per_hour * hour + std.time.s_per_min * minute },
    };
}

fn parseEvent(buf: []const u8) !Event {
    var fields = std.mem.tokenizeAny(u8, buf, " #");
    // FIXME?
    const first = fields.next() orelse return error.InvalidInput;
    if (std.mem.eql(u8, first, "Guard")) {
        const guard = try std.fmt.parseUnsigned(u16, fields.next() orelse return error.InvalidInput, 10);
        if (std.mem.eql(u8, fields.next() orelse "", "begins") and
            std.mem.eql(u8, fields.next() orelse "", "shift") and
            fields.next() == null)
        {
            return .{ .begins_shift = guard };
        } else {
            return error.InvalidInput;
        }
    } else if (std.mem.eql(u8, first, "falls") and
        std.mem.eql(u8, fields.next() orelse "", "asleep") and
        fields.next() == null)
    {
        return .falls_asleep;
    } else if (std.mem.eql(u8, first, "wakes") and
        std.mem.eql(u8, fields.next() orelse "", "up") and
        fields.next() == null)
    {
        return .wakes_up;
    } else {
        return error.InvalidInput;
    }
}

fn parseTimedEvent(buf: []const u8) !TimedEvent {
    var fields = std.mem.tokenizeAny(u8, buf, "[]");

    const timestamp = fields.next() orelse return error.InvalidInput;
    const event = fields.next() orelse return error.InvalidInput;
    if (fields.next() != null) {
        return error.InvalidInput;
    }

    return .{ try parseTimestamp(timestamp), try parseEvent(event) };
}

fn lessThanTimestamp(_: void, lhs: Timestamp, rhs: Timestamp) bool {
    return std.mem.order(usize, &[_]usize{
        lhs[0],
        lhs[1].month.numeric(),
        lhs[1].day_index,
        lhs[2].secs,
    }, &[_]usize{
        rhs[0],
        rhs[1].month.numeric(),
        rhs[1].day_index,
        rhs[2].secs,
    }) == .lt;
}

fn lessThanTimedEvent(_: void, lhs: TimedEvent, rhs: TimedEvent) bool {
    return lessThanTimestamp({}, lhs[0], rhs[0]);
}

const SleepDistrib = struct {
    total: usize = 0,
    per_minute: [60]usize = .{0} ** 60,
};

const GuardSleepDistrib = std.AutoHashMap(u16, SleepDistrib);

fn takeTimedEvents(allocator: std.mem.Allocator, reader: *std.Io.Reader) !std.ArrayList(TimedEvent) {
    var events: std.ArrayList(TimedEvent) = try .initCapacity(allocator, 1250);

    while (try reader.takeDelimiter('\n')) |line| {
        try events.append(allocator, try parseTimedEvent(line));
    }
    std.sort.block(TimedEvent, events.items, {}, lessThanTimedEvent);

    return events;
}

fn buildGuardSleepDistrib(allocator: std.mem.Allocator, events: []const TimedEvent) !GuardSleepDistrib {
    var distrib: GuardSleepDistrib = .init(allocator);
    var guard_on_duty: ?u16 = null;
    var falls_asleep: ?u6 = null;
    var wakes_up: ?u6 = null;

    for (events) |ev| {
        switch (ev[1]) {
            .begins_shift => |guard| {
                guard_on_duty = guard;
            },
            .falls_asleep => {
                std.debug.assert(guard_on_duty != null and
                    falls_asleep == null and
                    wakes_up == null);
                falls_asleep = ev[0][2].getMinutesIntoHour();
            },
            .wakes_up => {
                std.debug.assert(guard_on_duty != null and
                    falls_asleep != null and
                    wakes_up == null);
                wakes_up = ev[0][2].getMinutesIntoHour();

                const entry = try distrib.getOrPut(guard_on_duty.?);
                if (!entry.found_existing) {
                    entry.value_ptr.total = 0;
                    @memset(&entry.value_ptr.per_minute, 0);
                }
                for (falls_asleep.?..wakes_up.?) |min| {
                    entry.value_ptr.per_minute[min] += 1;
                    entry.value_ptr.total += 1;
                }

                wakes_up = null;
                falls_asleep = null;
            },
        }
    }

    return distrib;
}

pub fn part1(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const allocator = init.arena.allocator();

    const events = try takeTimedEvents(allocator, stdin);
    const distrib = try buildGuardSleepDistrib(allocator, events.items);

    // Strategy 1:
    var iter = distrib.iterator();
    var best_entry = iter.next() orelse return error.NoSolution;
    while (iter.next()) |entry| {
        if (entry.value_ptr.total > best_entry.value_ptr.total) {
            best_entry = entry;
        }
    }
    const min = std.sort.argMax(usize, &best_entry.value_ptr.per_minute, {}, std.sort.asc(usize)) orelse
        return error.NoSolution;

    try stdout.print("{d}\n", .{best_entry.key_ptr.* * min});
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const allocator = init.arena.allocator();

    const events = try takeTimedEvents(allocator, stdin);
    const distrib = try buildGuardSleepDistrib(allocator, events.items);

    // Strategy 2:
    var iter = distrib.iterator();
    var best_entry = iter.next() orelse return error.NoSolution;
    var best_min = std.sort.argMax(usize, &best_entry.value_ptr.per_minute, {}, std.sort.asc(usize)) orelse
        return error.NoSolution;
    while (iter.next()) |entry| {
        const min = std.sort.argMax(usize, &entry.value_ptr.per_minute, {}, std.sort.asc(usize)) orelse
            return error.NoSolution;
        if (entry.value_ptr.per_minute[min] > best_entry.value_ptr.per_minute[best_min]) {
            best_entry = entry;
            best_min = min;
        }
    }

    try stdout.print("{d}\n", .{best_entry.key_ptr.* * best_min});
}
