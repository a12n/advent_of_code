%% -*- mode: mercury -*-
:- module day_06_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, list, maybe, string, uint32.
:- import_module day_06, io_ext.

:- pred main_loop(maybe(string)::in, int::in, int::out, answer_set::in, answer_set::out) is semidet.
main_loop(MaybeLine, !Sum, !Group) :-
    ( (MaybeLine = yes("\n"); MaybeLine = no) ->
      %% Group separator. Count number of answers in the current group, start new group.
	  !:Sum   = !.Sum + num_ones(!.Group),
      !:Group = \0u32
    ; MaybeLine = yes(Line) ->
      string_answers(chomp(Line), Answers),
      !:Sum   = !.Sum,
      !:Group = !.Group /\ Answers
    ; fail
    ).

main(!IO) :-
    lines_foldl2(main_loop, 0, \0u32, Result, !IO),
    ( Result = ok(Sum),
      write_int(Sum, !IO), nl(!IO)
    ; Result = error(_, Error),
      error_exit(1, error_message(Error), !IO)
    ).
