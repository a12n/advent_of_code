%% -*- mode: mercury -*-
:- module day_10_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

:- import_module io_ext.

:- func solution(list(int)) = int is semidet.
solution(Numbers) = Result :-
    sort(Numbers, Sorted),

    last(Sorted, Max),
    Diffs = map_corresponding(
        minus,
        Sorted ++ [Max + 3],
        [0 | Sorted]
    ),

    filter(unify(1), Diffs, Diffs1),
    filter(unify(3), Diffs, Diffs3),

    Result = length(Diffs1) * length(Diffs3).

main(!IO) :-
    input_lines(Result, !IO),
    ( Result = ok(Lines),
      ( map(to_int, map(chomp, Lines), Numbers), N = solution(Numbers)->
        write_int(N, !IO), nl(!IO)
      ; error("Invalid input")
      )
    ; Result = error(Error),
      error(error_message(Error))
    ).
