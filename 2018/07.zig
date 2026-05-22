const std = @import("std");

const Graph = struct {
    const max_nodes = 'Z' - 'A' + 1;
    const AdjacencyMatrix = [max_nodes]?[max_nodes]?void;

    outgoing: AdjacencyMatrix = .{null} ** max_nodes,

    fn addEdge(self: *Graph, from: u8, to: u8) void {
        if (self.outgoing[from] == null) {
            self.outgoing[from] = .{null} ** max_nodes;
        }
        self.outgoing[from].?[to] = {};
    }

    fn hasIncoming(self: Graph, v: u8) bool {
        for (0..max_nodes) |u| {
            if (self.outgoing[u] != null and self.outgoing[u].?[v] != null) {
                return true;
            }
        }
        return false;
    }

    fn topologicalSort(self: Graph) NodeOrder {
        var order: NodeOrder = .{};
        var state: std.StaticBitSet(max_nodes) = .initEmpty();
        var copy = self;

        for (0..max_nodes) |u| {
            if (copy.outgoing[u] != null and !copy.hasIncoming(@intCast(u))) {
                state.setValue(u, true);
            }
        }

        while (state.findFirstSet()) |u| {
            state.setValue(u, false);
            order.addNode(@intCast(u));
            if (copy.outgoing[u] == null) {
                continue;
            }
            for (0..max_nodes) |v| {
                if (copy.outgoing[u].?[v] != null) {
                    copy.outgoing[u].?[v] = null;
                    if (!copy.hasIncoming(@intCast(v))) {
                        state.setValue(v, true);
                    }
                }
            }
        }

        return order;
    }

    const NodeOrder = struct {
        buf: [max_nodes]u8 = undefined,
        pos: usize = 0,

        fn addNode(self: *NodeOrder, node: u8) void {
            std.debug.assert(self.pos < max_nodes);
            self.buf[self.pos] = node;
            self.pos += 1;
        }

        fn toSlice(self: *const NodeOrder) []const u8 {
            return self.buf[0..self.pos];
        }
    };
};

fn parseNode(str: []const u8) !u8 {
    if (str.len != 1 or str[0] < 'A' or str[0] > 'Z') {
        return error.InvalidInput;
    }
    return str[0] - 'A';
}

fn parseEdge(str: []const u8) !struct { u8, u8 } {
    var fields = std.mem.tokenizeAny(u8, str, " .");
    if (!std.mem.eql(u8, fields.next() orelse "", "Step")) return error.InvalidInput;
    const from = try parseNode(fields.next() orelse return error.InvalidInput);
    if (!std.mem.eql(u8, fields.next() orelse "", "must") or
        !std.mem.eql(u8, fields.next() orelse "", "be") or
        !std.mem.eql(u8, fields.next() orelse "", "finished") or
        !std.mem.eql(u8, fields.next() orelse "", "before") or
        !std.mem.eql(u8, fields.next() orelse "", "step")) return error.InvalidInput;
    const to = try parseNode(fields.next() orelse return error.InvalidInput);
    if (!std.mem.eql(u8, fields.next() orelse "", "can") or
        !std.mem.eql(u8, fields.next() orelse "", "begin")) return error.InvalidInput;
    if (fields.next() != null) {
        return error.InvalidInput;
    }
    return .{ from, to };
}

fn readGraph(reader: *std.Io.Reader) !Graph {
    var graph: Graph = .{};
    while (try reader.takeDelimiter('\n')) |line| {
        const from, const to = try parseEdge(line);
        graph.addEdge(from, to);
    }
    return graph;
}

pub fn part1(_: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const graph = try readGraph(stdin);
    const order = graph.topologicalSort();
    for (order.toSlice()) |node| {
        try stdout.printAsciiChar(node + 'A', .{});
    }
    try stdout.printAsciiChar('\n', .{});
}

pub fn part2(init: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    _ = init;
    _ = stdin;
    _ = stdout;
    // TODO
}
