-module(seqs).

-type seq(Value) :: fun(() -> {Value, _Next :: seq(Value)} | undefined).
-export_type([seq/1]).

-export([
    empty/0,
    from_list/1,
    to_list/1,
    append/2,
    duplicate/2,
    cycle/2,
    filter/2,
    map/2,
    filtermap/2,
    fold/3,
    foreach/2,
    range/1,
    range/2,
    range/3
]).

-spec empty() -> seq(term()).
empty() -> fun() -> undefined end.

-spec from_list(list()) -> seq(term()).
from_list([]) -> empty();
from_list([Value | Tail]) -> fun() -> {Value, from_list(Tail)} end.

-spec to_list(seq(term())) -> list().
to_list(Seq) ->
    %% TODO: Tail-recursive?
    case Seq() of
        undefined -> [];
        {Value, Next} -> [Value | to_list(Next)]
    end.

-spec append(seq(term()), seq(term())) -> seq(term()).
append(Seq1, Seq2) ->
    case Seq1() of
        undefined -> Seq2;
        {Value, Next} -> fun() -> {Value, append(Next, Seq2)} end
    end.

-spec duplicate(non_neg_integer() | infinity, term()) -> seq(term()).
duplicate(infinity, Value) -> fun() -> {Value, duplicate(infinity, Value)} end;
duplicate(0, _) -> empty();
duplicate(N, Value) when N > 0 -> fun() -> {Value, duplicate(N - 1, Value)} end.

-spec cycle(non_neg_integer() | infinity, seq(term())) -> seq(term()).
cycle(0, _) ->
    empty();
cycle(1, Seq) ->
    Seq;
cycle(N, InitialSeq) ->
    case InitialSeq() of
        undefined ->
            empty();
        {_, _} ->
            (fun
                Loop(_, I) when I >= N ->
                    empty();
                Loop(Seq, I) ->
                    case Seq() of
                        undefined -> Loop(InitialSeq, I + 1);
                        {Value, Next} -> fun() -> {Value, Loop(Next, I)} end
                    end
            end)(
                InitialSeq, 0
            )
    end.

-spec filter(fun((term()) -> boolean()), seq(term())) -> seq(term()).
filter(Pred, Seq) -> filtermap(fun(Value) -> {Pred(Value), Value} end, Seq).

-spec map(fun((term()) -> term()), seq(term())) -> seq(term()).
map(Fun, Seq) -> filtermap(fun(Value) -> {true, Fun(Value)} end, Seq).

%% FIXME
-spec filtermap(fun((term()) -> {true | false, term()} | boolean()), seq(term())) -> seq(term()).
filtermap(Fun, Seq) ->
    fun() ->
        case Seq() of
            undefined ->
                undefined;
            {Value, Next} ->
                case Fun(Value) of
                    false -> (filtermap(Fun, Next))();
                    {false, _} -> (filtermap(Fun, Next))();
                    true -> {Value, filtermap(Fun, Next)};
                    {true, Value2} -> {Value2, filtermap(Fun, Next)}
                end
        end
    end.

-spec fold(fun((term(), term()) -> term()), term(), seq(term())) -> term().
fold(Fun, Acc, Seq) ->
    case Seq() of
        undefined -> Acc;
        {Value, Next} -> fold(Fun, Fun(Value, Acc), Next)
    end.

-spec foreach(fun(), seq(term())) -> ok.
foreach(Fun, Seq) ->
    case Seq() of
        undefined ->
            ok;
        {Value, Next} ->
            Fun(Value),
            foreach(Fun, Next)
    end.

-spec range(integer()) -> seq(integer()).
range(From) -> range(From, infinity).

-spec range(integer(), integer() | infinity) -> seq(integer()).
range(From, To) when From =< To -> range(From, To, 1).

-spec range(integer(), integer() | infinity, neg_integer() | pos_integer()) -> seq(integer()).
range(From, infinity, Incr) -> fun() -> {From, range(From + Incr, infinity, Incr)} end;
range(From, To, Incr) when Incr > 0, From > To -> empty();
range(From, To, Incr) when Incr < 0, From < To -> empty();
range(From, To, Incr) -> fun() -> {From, range(From + Incr, To, Incr)} end.
