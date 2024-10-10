-module(graphviz).

-export([to_iodata/1, to_iodata/2]).

-spec to_iodata(map()) -> iodata().
to_iodata(Graph) -> to_iodata(Graph, #{}).

-spec to_iodata(map(), map()) -> iodata().
to_iodata(Graph, GraphMap) ->
    EdgeAttrs = [label, xlabel],
    GraphAttrs = [label],
    NodeAttrs = [fillcolor, label, shape, xlabel],
    [
        <<"digraph {\n">>,
        attrs_to_iodata(maps:with(GraphAttrs, GraphMap), ";\n"), $\n,
        lists:map(
            fun({U, Node}) ->
                StrU = value_to_iodata(U),
                [
                    "\t\"",
                    StrU,
                    "\" [",
                    attrs_to_iodata(maps:with(NodeAttrs, Node)),
                    "];\n",
                    lists:map(
                        fun({V, Edge}) ->
                            StrV = value_to_iodata(V),
                            [
                                "\t\"",
                                StrU,
                                "\" -> \"",
                                StrV,
                                "\" [",
                                attrs_to_iodata(
                                    maps:with(
                                        EdgeAttrs,
                                        case is_map(Edge) of
                                            true -> Edge;
                                            false -> #{label => value_to_iodata(Edge)}
                                        end
                                    )
                                ),
                                "];\n"
                            ]
                        end,
                        lists:sort(
                            case maps:get(adjacent, Node, []) of
                                List when is_list(List) -> lists:map(fun(V) -> {V, #{}} end, List);
                                Map when is_map(Map) -> maps:to_list(Map)
                            end
                        )
                    )
                ]
            end,
            lists:sort(maps:to_list(Graph))
        ),
        <<"}\n">>
    ].

-spec attrs_to_iodata(map()) -> iodata().
attrs_to_iodata(Attrs) -> attrs_to_iodata(Attrs, ", ").

-spec attrs_to_iodata(map(), iodata()) -> iodata().
attrs_to_iodata(Attrs, Sep) ->
    lists:join(
        Sep,
        lists:map(
            fun({Key, Value}) ->
                [value_to_iodata(Key), "=\"", value_to_iodata(Value), "\""]
            end,
            lists:sort(maps:to_list(Attrs))
        )
    ).

-spec value_to_iodata(number() | atom() | binary() | string()) -> iodata().
value_to_iodata(N) when is_integer(N) -> integer_to_binary(N);
value_to_iodata(X) when is_float(X) -> float_to_binary(X);
value_to_iodata(Atom) when is_atom(Atom) -> atom_to_binary(Atom);
value_to_iodata(Str) when is_binary(Str) -> Str;
value_to_iodata(Str) when is_list(Str) -> Str.
