-module(segments).

-type t() :: {_Min :: integer(), _Max :: integer()}.
-export_type([t/0]).

-define(IS_EMPTY(Min, Max), Min > Max).
-define(IS_DISJOINT(Min1, Max1, Min2, Max2), Min2 > Max1 orelse Min1 > Max2).
-define(IS_SUBSET(Min1, Max1, Min2, Max2), Min1 >= Min2 andalso Max1 =< Max2).

-export([
    empty/0,
    from_endpoints/2,
    from_integer/1,
    from_length/2,
    is_disjoint/2,
    is_element/2,
    is_empty/1,
    is_subset/2,
    size/1,
    intersection/2,
    merge/1,
    merge/2,
    union/2,
    subtract/2
]).

-spec empty() -> t().
empty() -> {0, -1}.

-spec from_endpoints(integer(), integer()) -> t().
from_endpoints(Min, Max) when Min =< Max -> {Min, Max}.

-spec from_integer(integer()) -> t().
from_integer(N) -> {N, N}.

-spec from_length(integer(), non_neg_integer()) -> t().
from_length(Min, 0) -> {Min, Min};
from_length(Min, N) when N > 0 -> {Min, Min + N - 1}.

-spec size(t()) -> non_neg_integer().
size({Min, Max}) -> max(0, Max - Min + 1).

-spec is_disjoint(t(), t()) -> boolean().
is_disjoint({Min1, Max1}, {Min2, Max2}) -> ?IS_DISJOINT(Min1, Max1, Min2, Max2).

-spec is_element(integer(), t()) -> boolean().
is_element(Elt, {Min, Max}) -> Elt >= Min andalso Elt =< Max.

-spec is_empty(t()) -> boolean().
is_empty({Min, Max}) -> ?IS_EMPTY(Min, Max).

-spec is_subset(t(), t()) -> boolean().
is_subset({Min1, Max1}, {Min2, Max2}) -> ?IS_SUBSET(Min1, Max1, Min2, Max2).

-spec intersection(t(), t()) -> t().
intersection({Min1, Max1}, {Min2, Max2}) when ?IS_DISJOINT(Min1, Max1, Min2, Max2) -> empty();
intersection({Min1, Max1}, {Min2, Max2}) -> {max(Min1, Min2), min(Max1, Max2)}.

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

-spec union(t(), t()) -> t() | [t()].
union(S1 = {Min1, Max1}, S2 = {Min2, Max2}) ->
    case merge(S1, S2) of
        S3 = {_, _} -> S3;
        undefined when Max1 < Min2 -> [S1, S2];
        undefined when Max2 < Min1 -> [S2, S1]
    end.

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
    end.
