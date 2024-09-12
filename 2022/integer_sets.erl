-module(integer_sets).

-type t() :: [segments:t()].
-export_type([t/0]).

-export([
    empty/0,
    from_list/1,
    from_segment/1,
    from_segments/1,
    is_element/2,
    is_empty/1,
    is_subset/2,
    size/1,
    intersection/2,
    union/2,
    subtract/2
]).

-spec empty() -> t().
empty() -> [].

-spec from_list([integer()]) -> t().
from_list(List) -> from_segments([segments:from_integer(N) || N <- List]).

-spec from_segment(segments:t()) -> t().
from_segment(S) -> [S].

-spec from_segments([segments:t()]) -> t().
from_segments(Segments) -> normalize(segments:merge(lists:sort(Segments))).

-spec size(t()) -> non_neg_integer().
size(Segments) ->
    lists:foldl(fun(S, Size) -> segments:size(S) + Size end, 0, Segments).

-spec is_element(integer(), t()) -> boolean().
is_element(Elt, Segments) when is_list(Segments) ->
    lists:any(fun(S) -> segments:is_element(Elt, S) end, Segments).

-spec is_empty(t()) -> boolean().
is_empty(Segments) -> lists:all(fun segments:is_empty/1, Segments).

%% @doc
%% Is `S1` a subset of `S2`?
%% @end
-spec is_subset(t(), t()) -> boolean().
is_subset(Segments1, Segments2) ->
    lists:all(fun(S1) -> lists:any(fun(S2) -> is_subset(S1, S2) end, Segments2) end, Segments1).

-spec intersection(t(), t()) -> t().
intersection([], _) ->
    empty();
intersection(_, []) ->
    empty();
intersection(Segments1 = [_ | _], Segments2 = [_ | _]) ->
    normalize(
        segments:merge(
            lists:sort(
                lists:flatten([segments:intersection(S1, S2) || S1 <- Segments1, S2 <- Segments2])
            )
        )
    ).

-spec union(t(), t()) -> t().
union([], Segments2) ->
    Segments2;
union(Segments1, []) ->
    Segments1;
union(Segments1 = [_ | _], Segments2 = [_ | _]) ->
    normalize(segments:merge(lists:sort(lists:append(Segments1, Segments2)))).

-spec subtract(t(), t()) -> t().
subtract(Segments1, []) ->
    Segments1;
subtract([], _) ->
    empty();
subtract(Segments1 = [_ | _], Segments2 = [_ | _]) ->
    normalize(
        segments:merge(
            lists:sort(
                lists:flatten([segments:subtract(S1, S2) || S1 <- Segments1, S2 <- Segments2])
            )
        )
    ).

%% @doc
%% Converts result of segments:merge/1 to integer set of type t/0.
%% @end
-spec normalize(segments:t() | [segments:t()]) -> t().
normalize(S = {_, _}) ->
    case segments:is_empty(S) of
        true -> [];
        false -> [S]
    end;
normalize(Segments = [_ | _]) ->
    Segments.
