-module(day_13).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-spec main(1..2) -> ok.
main(Part) ->
    Terms =
        lists:map(
            fun(TermStr) ->
                {ok, Tokens, _} = erl_scan:string(binary_to_list(TermStr)),
                {ok, Term} = erl_parse:parse_term(Tokens),
                Term
            end,
            lists:filtermap(
                fun
                    (<<>>) -> false;
                    (<<Line/bytes>>) -> {true, <<Line/bytes, $.>>}
                end,
                io_ext:read_lines(standard_io)
            )
        ),
    case Part of
        1 ->
            PairIndexList = lists:filtermap(
                fun({Index, {A, B}}) ->
                    case compare(A, B) of
                        K when K < 0 -> {true, Index};
                        _ -> false
                    end
                end,
                lists:enumerate(lists_ext:consec_pairs(Terms))
            ),
            io:format(<<"~b~n">>, [lists:sum(PairIndexList)]);
        2 ->
            [Index1, Index2] = lists:filtermap(
                fun
                    ({Index, [[2]]}) -> {true, Index};
                    ({Index, [[6]]}) -> {true, Index};
                    (_) -> false
                end,
                lists:enumerate(
                    lists:sort(
                        fun(A, B) ->
                            compare(A, B) =< 0
                        end,
                        [[[2]], [[6]] | Terms]
                    )
                )
            ),
            io:format(<<"~b~n">>, [Index1 * Index2])
    end.

-spec compare(integer() | [integer()], integer() | [integer()]) -> integer().
compare(A, B) when is_integer(A), is_integer(B) -> A - B;
compare(A, B) when is_integer(A), is_list(B) -> compare([A], B);
compare(A, B) when is_list(A), is_integer(B) -> compare(A, [B]);
compare([], [_ | _]) ->
    -1;
compare([_ | _], []) ->
    +1;
compare([], []) ->
    0;
compare([A0 | A], [B0 | B]) ->
    case compare(A0, B0) of
        0 -> compare(A, B);
        K -> K
    end.
