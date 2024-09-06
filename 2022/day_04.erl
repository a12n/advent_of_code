-module(day_04).

-export([main/1]).

-spec main(1..2) -> ok.
main(Part) ->
    SegmentPairs =
        lists:map(fun binary_to_segment_pair/1, io_ext:read_lines(standard_io)),
    Predicate =
        case Part of
            1 ->
                fun({First, Second}) ->
                    segment:is_subset(First, Second) orelse
                        segment:is_subset(Second, First)
                end;
            2 ->
                fun({First, Second}) ->
                    not segment:is_disjoint(First, Second)
                end
        end,
    io:format(<<"~b~n">>, [length(lists:filter(Predicate, SegmentPairs))]).

-spec binary_to_segment_pair(binary()) -> {segment:t(), segment:t()}.
binary_to_segment_pair(Str) ->
    [First, Second] = binary:split(Str, <<",">>),
    {binary_to_segment(First), binary_to_segment(Second)}.

-spec binary_to_segment(binary()) -> segment:t().
binary_to_segment(Str) ->
    [Min, Max] = binary:split(Str, <<"-">>),
    segment:from_endpoints(binary_to_integer(Min), binary_to_integer(Max)).
