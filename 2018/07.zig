const std = @import("std");

const Graph = struct {
    const max_nodes = 'Z' - 'A' + 1;
    const AdjacencyMatrix = [max_nodes]?[max_nodes]?void;

    edges: AdjacencyMatrix = .{null} ** max_nodes,

    fn addEdge(self: *Graph, from: u8, to: u8) void {
        if (self.edges[from] == null) {
            self.edges[from] = .{null} ** max_nodes;
        }
        self.edges[from].?[to] = {};
    }

    fn _visitNode(
        self: Graph,
        order: *NodeOrder,
        visited: *[max_nodes]bool,
        node: u8,
    ) void {
        std.debug.print("_visitNode: node {c}, visited {any}\n", .{ node + 'A', visited[node] });
        visited[node] = if (!visited[node]) true else return;
        if (self.edges[node]) |adjacent| {
            var adj_node: u8 = max_nodes;
            while (adj_node > 0) : (adj_node -= 1) {
                if (adjacent[adj_node - 1] != null) {
                    self._visitNode(order, visited, adj_node - 1);
                }
            }
        }
        order.addNode(node);
    }

    fn topologicalSort(self: Graph) NodeOrder {
        var order: NodeOrder = .{};
        var visited: [max_nodes]bool = .{false} ** max_nodes;

        var node: u8 = max_nodes;
        while (node > 0) : (node -= 1) {
            if (self.edges[node - 1] != null) {
                self._visitNode(&order, &visited, node - 1);
            }
        }

        return order;
    }

    const NodeOrder = struct {
        buf: [max_nodes]u8 = undefined,
        pos: usize = max_nodes,

        fn addNode(self: *NodeOrder, node: u8) void {
            std.debug.assert(self.pos > 0);
            self.pos -= 1;
            self.buf[self.pos] = node;
        }

        fn toSlice(self: *const NodeOrder) []const u8 {
            return self.buf[self.pos..];
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
