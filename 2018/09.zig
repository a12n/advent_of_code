const std = @import("std");

const List = std.DoublyLinkedList;

// TODO: Optimize for memory. Instead of individual nodes and
// prev/next pointers (8 bytes each), use array of marbles and back/fwd
// offsets (1 byte each).
const Marble = struct {
    number: usize,
    node: List.Node = .{},
};

fn cw(list: List, from_node: *List.Node, steps: usize) *List.Node {
    var node = from_node;
    for (0..steps) |_| {
        if (node.next) |next| {
            node = next;
        } else if (list.first) |first| {
            node = first;
        }
    }
    return node;
}

fn ccw(list: List, from_node: *List.Node, steps: usize) *List.Node {
    var node = from_node;
    for (0..steps) |_| {
        if (node.prev) |prev| {
            node = prev;
        } else if (list.last) |last| {
            node = last;
        }
    }
    return node;
}

fn parseGame(str: []const u8) !struct { usize, usize } {
    var fields = std.mem.tokenizeAny(u8, str, "players; last marble is worth points");
    const n_players = try std.fmt.parseUnsigned(usize, fields.next() orelse return error.InvalidInput, 10);
    const last_marble = try std.fmt.parseUnsigned(usize, fields.next() orelse return error.InvalidInput, 10);
    if (fields.next() != null) return error.InvalidInput;
    return .{ n_players, last_marble };
}

fn readGame(reader: *std.Io.Reader) !struct { usize, usize } {
    return try parseGame(try reader.takeDelimiter('\n') orelse return error.InvalidInput);
}

// Returns winning player's score. Since allocator is arena allocator,
// no free/destroy in this function.
fn marbleGame(allocator: std.mem.Allocator, n_players: usize, last_marble: usize) !usize {
    var marble: usize = 0;
    var player: usize = 0;
    var score = try allocator.alloc(usize, n_players);
    @memset(score, 0);

    var list: List = .{};
    var current: *Marble = try allocator.create(Marble);

    current.* = .{ .number = marble };
    list.append(&current.node);
    marble += 1;

    while (marble <= last_marble) : (marble += 1) {
        if (marble % 23 == 0) {
            const removed: *Marble = @fieldParentPtr("node", ccw(list, &current.node, 7));
            if (removed.node.next) |next| {
                current = @fieldParentPtr("node", next);
            } else if (list.first) |first| {
                current = @fieldParentPtr("node", first);
            } else {
                unreachable;
            }
            list.remove(&removed.node);
            score[player] += marble;
            score[player] += removed.number;
        } else {
            const new: *Marble = try allocator.create(Marble);
            new.* = .{ .number = marble };
            list.insertAfter(cw(list, &current.node, 1), &new.node);
            current = new;
        }
        player = (player + 1) % n_players;
    }

    return score[std.mem.findMax(usize, score)];
}

pub fn part1(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const allocator = init.arena.allocator();
    const n_players, const last_marble = try readGame(stdin);
    try stdout.print("{d}\n", .{try marbleGame(allocator, n_players, last_marble)});
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const allocator = init.arena.allocator();
    const n_players, const last_marble = try readGame(stdin);
    try stdout.print("{d}\n", .{try marbleGame(allocator, n_players, 100 * last_marble)});
}
