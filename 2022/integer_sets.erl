-module(integer_sets).

-type t() :: [segments:t()].
-export_type([t/0]).

-export([
    empty/0,
    from_list/1,
    from_segment/1
    is_disjoint/2,
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

-spec from_endpoints(integer(), integer()) -> t().
from_endpoints(Min, Max) when Min =< Max -> {Min, Max}.

-spec from_length(integer(), non_neg_integer()) -> t().
from_length(Min, 0) -> {Min, Min};
from_length(Min, N) when N > 0 -> {Min, Min + N - 1}.

%% @doc
%% List of set members.
%% @end
-spec to_list(t() | [t()]) -> [integer()].
to_list({Min, Max}) ->
    (fun
        Loop(I, List) when I < Min -> List;
        Loop(I, List) -> Loop(I - 1, [I | List])
    end)(
        Max, []
    );
to_list(Segments) when is_list(Segments) -> lists:flatten(lists:map(fun to_list/1, Segments)).

-spec size(t() | [t()]) -> non_neg_integer().
size({Min, Max}) ->
    max(0, Max - Min + 1);
size(Segments) when is_list(Segments) ->
    lists:foldl(
        fun(S, Size) ->
            ?MODULE:size(S) + Size
        end,
        0,
        Segments
    ).

-spec is_disjoint(t() | [t()], t() | [t()]) -> boolean().
%% TODO: Lists
is_disjoint({Min1, Max1}, {Min2, Max2}) -> ?IS_DISJOINT(Min1, Max1, Min2, Max2).

-spec is_element(integer(), t() | [t()]) -> boolean().
is_element(Elt, {Min, Max}) ->
    Elt >= Min andalso Elt =< Max;
is_element(Elt, Segments) when is_list(Segments) ->
    lists:any(fun(S) -> is_element(Elt, S) end, Segments).

-spec is_empty(t() | [t()]) -> boolean().
is_empty({Min, Max}) -> ?IS_EMPTY(Min, Max);
is_empty([]) -> true;
is_empty(Segments = [_ | _]) -> lists:all(fun is_empty/1, Segments).

%% @doc
%% Is `S1` a subset of `S2`?
%% @end
-spec is_subset(t() | [t()], t() | [t()]) -> boolean().
is_subset({Min1, Max1}, {Min2, Max2}) ->
    ?IS_SUBSET(Min1, Max1, Min2, Max2);
is_subset(Segments1, S2 = {_, _}) when is_list(Segments1) ->
    lists:all(fun(S1) -> is_subset(S1, S2) end, Segments1);
is_subset(S1 = {_, _}, Segments2) when is_list(Segments2) ->
    lists:any(fun(S2) -> is_subset(S1, S2) end, Segments2);
is_subset(Segments1, Segments2) when is_list(Segments1), is_list(Segments2) ->
    lists:all(fun(S1) -> lists:any(fun(S2) -> is_subset(S1, S2) end, Segments2) end, Segments1).

-spec intersection(t() | [t()], t() | [t()]) -> t() | [t()].
intersection({Min1, Max1}, {Min2, Max2}) when ?IS_DISJOINT(Min1, Max1, Min2, Max2) -> empty();
intersection({Min1, Max1}, {Min2, Max2}) ->
    {max(Min1, Min2), min(Max1, Max2)};
intersection([], _) ->
    empty();
intersection(_, []) ->
    empty();
intersection(Segments1 = [_ | _], S2 = {_, _}) ->
    merge(
        lists:filtermap(
            fun(S1) ->
                case intersection(S1, S2) of
                    {Min, Max} when ?IS_EMPTY(Min, Max) -> false;
                    S3 = {_, _} -> {true, S3}
                end
            end,
            Segments1
        )
    );
intersection(S1 = {_, _}, Segments2 = [_ | _]) ->
    intersection(Segments2, S1);
intersection(Segments1 = [_ | _], Segments2 = [_ | _]) ->
    %% TODO
    [].

%% @doc
%% Like union/2, but works only for non-list segments and only merges
%% intersecting/touching segments. If segments are disjoint, returns
%% `undefined`.
%% @end
-spec merge(t(), t()) -> t() | undefined.
merge({Min1, Max1}, S2) when ?IS_EMPTY(Min1, Max1) -> S2;
merge(S1, {Min2, Max2}) when ?IS_EMPTY(Min2, Max2) -> S1;
merge({Min1, Max1}, {Min2, Max2}) when ?IS_DISJOINT(Min1, Max1, Min2, Max2) ->
    if
        (Max1 + 1) == Min2 -> {Min1, Max2};
        (Max2 + 1) == Min1 -> {Min2, Max1};
        true -> undefined
    end;
merge({Min1, Max1}, {Min2, Max2}) ->
    {min(Min1, Min2), max(Max1, Max2)}.

%% @doc
%% Tries to merge/2 subsequent segments in a sorted list.
%% @end
-spec merge([t()]) -> t() | [t()].
merge([]) ->
    empty();
merge([S1]) ->
    S1;
merge([S1 | SegmentsPast1 = [S2 | SegmentsPast2]]) ->
    case merge(S1, S2) of
        undefined -> [S1 | merge(SegmentsPast1)];
        S3 -> merge([S3 | SegmentsPast2])
    end.

-spec union(t() | [t()], t() | [t()]) -> t() | [t()].
union(S1 = {Min1, Max1}, S2 = {Min2, Max2}) ->
    case merge(S1, S2) of
        S3 = {_, _} -> S3;
        undefined when Max1 < Min2 -> [S1, S2];
        undefined when Max2 < Min1 -> [S2, S1]
    end;
union(Segments1, S2 = {_, _}) when is_list(Segments1) -> union(Segments1, [S2]);
union(S1 = {_, _}, Segments2) when is_list(Segments2) -> union([S1], Segments2);
union(Segments1, Segments2) when is_list(Segments1), is_list(Segments2) ->
    merge(lists:sort(lists:append(Segments1, Segments2))).

-spec subtract(t(), t()) -> t() | [t()].
subtract(S1 = {Min1, Max1}, S2) ->
    case intersection(S1, S2) of
        {Min, Max} when ?IS_EMPTY(Min, Max) -> S1;
        {Min, Max} ->
            LeftOfS2 =
                if
                    Min > Min1 -> {Min1, Min - 1};
                    true -> undefined
                end,
            RightOfS2 =
                if
                    Max < Max1 -> {Max + 1, Max1};
                    true -> undefined
                end,
            case {LeftOfS2, RightOfS2} of
                {undefined, undefined} -> empty();
                {{_, _}, undefined} -> LeftOfS2;
                {undefined, {_, _}} -> RightOfS2;
                {{_, _}, {_, _}} -> [LeftOfS2, RightOfS2]
            end
    end;
subtract(S1, []) ->
    S1;
subtract([], _) ->
    empty();
subtract(Segments1 = [_ | _], S2 = {_, _}) ->
    subtract(Segments1, [S2]);
subtract(S1 = {_, _}, Segments2 = [_ | _]) ->
    subtract([S1], Segments2);
subtract(Segments1 = [_ | _], Segments2 = [_ | _]) ->
    merge(lists:sort(lists:flatten([subtract(S1, S2) || S1 <- Segments1, S2 <- Segments2]))).
