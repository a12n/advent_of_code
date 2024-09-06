-module(day_04).

-export([main/1]).

-spec main(1..2) -> ok.
main(Part) ->
    AllPairs =
        lists:map(fun binary_to_segment_pair/1, advent:file_lines(standard_io)),
    FilteredPairs =
        case Part of
            1 ->
                lists:filter(
                    fun({First, Second}) ->
                        segment:is_subset(First, Second) orelse
                            segment:is_subset(Second, First)
                    end,
                    AllPairs
                );
            2 ->
                %% TODO
                []
        end,
    io:format(<<"~b~n">>, [length(FilteredPairs)]).

-spec binary_to_segment_pair(binary()) -> {segment:t(), segment:t()}.
binary_to_segment_pair(Str) ->
    [First, Second] = binary:split(Str, <<",">>),
    {binary_to_segment(First), binary_to_segment(Second)}.

-spec binary_to_segment(binary()) -> segment:t().
binary_to_segment(Str) ->
    [Min, Max] = binary:split(Str, <<"-">>),
    segment:from_endpoints(binary_to_integer(Min), binary_to_integer(Max)).
