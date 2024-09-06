-module(day_04).

-export([main/1]).

-spec main(1..2) -> ok.
main(Part) ->
    SegmentPairs =
        case Part of
            1 ->
                lists:filtermap(
                    fun(Line) ->
                        {First, Second} = binary_to_segment_pair(Line),
                        segment:is_subset(First, Second) orelse
                            segment:is_subset(Second, First)
                    end,
                    advent:file_lines(standard_io)
                )
        end,
    io:format(<<"~b~n">>, [length(SegmentPairs)]).

-spec binary_to_segment_pair(binary()) -> {segment:t(), segment:t()}.
binary_to_segment_pair(Str) ->
    [First, Second] = binary:split(Str, <<",">>),
    {binary_to_segment(First), binary_to_segment(Second)}.

-spec binary_to_segment(binary()) -> segment:t().
binary_to_segment(Str) ->
    [Min, Max] = binary:split(Str, <<"-">>),
    segment:from_endpoints(binary_to_integer(Min), binary_to_integer(Max)).
