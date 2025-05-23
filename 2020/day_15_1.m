%% -*- mode: mercury -*-
:- module day_15_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module assoc_list.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.

main(!IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      ( map(to_int, split_at_char(',', chomp(Line)), Numbers) ->
        write_int(memory_game(Numbers), !IO), nl(!IO)
      ; error("Invalid input")
      )
    ; ReadResult = eof,
      error("EOF")
    ; ReadResult = error(Error),
      error(error_message(Error))
    ).

:- pred insert(int::in, int::in, int::out, int::in, int::out, map(int, list(int))::in, map(int, list(int))::out) is det.
insert(N, !Turn, !Last, !Spoken) :-
    set(N, [!.Turn], !Spoken),
    !:Last = N,
    !:Turn = !.Turn + 1.

:- func memory_game(list(int)) = int.
memory_game(Numbers) = Final :-
    foldl3(insert, Numbers, 1, Turn, -1, LastInit, init, Spoken),
    memory_game(Turn, LastInit, Final, Spoken, _).

:- pred memory_game(int::in, int::in, int::out, map(int, list(int))::in, map(int, list(int))::out) is det.
memory_game(Turn, !Last, !Spoken) :-
    %% trace [io(!IO)] io.format("Turn %d, Last %d, Spoken %s\n", [i(Turn), i(!.Last), s(string(to_assoc_list(!.Spoken) : assoc_list(int, list(int))))], !IO),
    Next = ( search(!.Spoken, !.Last, [Turn2, Turn1]) -> Turn2 - Turn1; 0 ),
    %% trace [io(!IO)] io.format("Next %d\n", [i(Next)], !IO),
    ( search(!.Spoken, Next, [Turn3 | _]) ->
      set(Next, [Turn, Turn3], !Spoken)
    ; set(Next, [Turn], !Spoken)
    ),
    !:Last = Next,
    ( Turn < 2020 -> memory_game(Turn + 1, !Last, !Spoken)
    ; true
    ).
