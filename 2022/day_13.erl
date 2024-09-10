-module(day_13).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
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
    Indices = lists:filtermap(
        fun({Index, {A, B}}) ->
            %% ?debugFmt("Index ~p, compare ~p ~p", [Index, A, B]),
            case compare(A, B) of
                -1 -> {true, Index};
                _ -> false
            end
        end,
        lists:enumerate(lists_ext:consec_pairs(Terms))
    ),
    io:format(<<"~b~n">>, [lists:sum(Indices)]).

-spec compare(integer() | [integer()], integer() | [integer()]) -> -1..1.
compare(A, B) when is_integer(A), is_integer(B) ->
    %% ?debugFmt("compare ~p ~p", [A, B]),
    if
        A < B -> -1;
        A > B -> +1;
        A == B -> 0
    end;
compare(A, B) when is_integer(A), is_list(B) ->
    %% ?debugFmt("compare ~p ~p", [A, B]),
    compare([A], B);
compare(A, B) when is_list(A), is_integer(B) ->
    %% ?debugFmt("compare ~p ~p", [A, B]),
    compare(A, [B]);
compare(A = [], B = [_ | _]) ->
    %% ?debugFmt("compare ~p ~p", [A, B]),
    -1;
compare(A = [_ | _], B = []) ->
    %% ?debugFmt("compare ~p ~p", [A, B]),
    +1;
compare(A = [], B = []) ->
    %% ?debugFmt("compare ~p ~p", [A, B]),
    0;
compare([A0 | A], [B0 | B]) ->
    %% ?debugFmt("compare ~p ~p", [[A0 | A], [B0 | B]]),
    case compare(A0, B0) of
        -1 -> -1;
        0 -> compare(A, B);
        +1 -> +1
    end.
