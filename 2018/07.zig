const std = @import("std");

const Graph = struct {
    const max_nodes = 'Z' - 'A' + 1;

    nodes: [max_nodes]?struct {
        incoming: std.StaticBitSet(max_nodes) = .initEmpty(),
        outgoing: std.StaticBitSet(max_nodes) = .initEmpty(),
    } = .{null} ** max_nodes,

    fn addEdge(self: *Graph, from: u8, to: u8) void {
        if (self.nodes[from] == null) {
            self.nodes[from] = .{};
        }
        if (self.nodes[to] == null) {
            self.nodes[to] = .{};
        }
        self.nodes[from].?.outgoing.setValue(to, true);
        self.nodes[to].?.incoming.setValue(from, true);
    }

    fn hasNode(self: Graph, u: u8) bool {
        return self.nodes[u] != null;
    }

    fn hasEdge(self: Graph, from: u8, to: u8) bool {
        return self.nodes[from].?.outgoing.isSet(to);
    }

    fn removeEdge(self: *Graph, from: u8, to: u8) void {
        self.nodes[from].?.outgoing.setValue(to, false);
        self.nodes[to].?.incoming.setValue(from, false);
    }

    fn hasIncoming(self: Graph, u: u8) bool {
        return self.nodes[u] != null and !self.nodes[u].?.incoming.eql(.empty);
    }

    fn topologicalSort(self: Graph) NodeOrder {
        var order: NodeOrder = .{};
        var queue: std.StaticBitSet(max_nodes) = .initEmpty();
        var graph = self;

        for (0..max_nodes) |u| {
            if (graph.hasNode(@intCast(u)) and !graph.hasIncoming(@intCast(u))) {
                queue.setValue(u, true);
            }
        }

        while (queue.findFirstSet()) |u| {
            queue.setValue(u, false);
            order.addNode(@intCast(u));
            for (0..max_nodes) |v| {
                if (graph.hasEdge(@intCast(u), @intCast(v))) {
                    graph.removeEdge(@intCast(u), @intCast(v));
                    if (!graph.hasIncoming(@intCast(v))) {
                        queue.setValue(v, true);
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

    var queue: std.StaticBitSet(Graph.max_nodes) = .initEmpty();
    var completed: std.StaticBitSet(Graph.max_nodes) = .initEmpty();

    for (0..Graph.max_nodes) |u| {
        if (graph.hasNode(@intCast(u)) and !graph.hasIncoming(@intCast(u))) {
            queue.setValue(u, true);
        }
    }

    while (!queue.eql(.empty)) {
        for (0..Graph.max_nodes) |u| {
            if (!queue.isSet(u)) {
                continue;
            }

            const node = graph.nodes[u] orelse continue;

            if (!completed.intersectWith(node.incoming).eql(node.incoming)) {
                continue;
            }

            queue.setValue(u, false);
            queue.setUnion(node.outgoing);
            try stdout.printAsciiChar(@as(u8, @intCast(u)) + 'A', .{});

            completed.setValue(u, true);
            break;
        }
    }
    try stdout.printAsciiChar('\n', .{});
}

pub fn part2(_: std.process.Init, stdin: *std.Io.Reader, stdout: *std.Io.Writer) !void {
    const graph = try readGraph(stdin);

    var queue: std.StaticBitSet(Graph.max_nodes) = .initEmpty();
    var completed: std.StaticBitSet(Graph.max_nodes) = .initEmpty();

    for (0..Graph.max_nodes) |u| {
        if (graph.hasNode(@intCast(u)) and !graph.hasIncoming(@intCast(u))) {
            queue.setValue(u, true);
        }
    }

    while (!queue.eql(.empty)) {
        for (0..Graph.max_nodes) |u| {
            if (!queue.isSet(u)) {
                continue;
            }

            const node = graph.nodes[u] orelse continue;

            if (!completed.intersectWith(node.incoming).eql(node.incoming)) {
                continue;
            }

            queue.setValue(u, false);
            queue.setUnion(node.outgoing);
            try stdout.printAsciiChar(@as(u8, @intCast(u)) + 'A', .{});

            completed.setValue(u, true);
            break;
        }
    }
    try stdout.printAsciiChar('\n', .{});
}
