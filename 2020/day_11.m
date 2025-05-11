%% -*- mode: mercury -*-
:- module day_11.
:- interface.

:- import_module array2d.
:- import_module io.
:- import_module maybe.

:- type seat ---> empty; occupied.
:- type seat_layout == array2d(maybe(seat)).

:- pred input_seat_layout(res(seat_layout)::out, io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.

:- import_module io_ext.

:- pred seat_char(char, maybe(seat)).
:- mode seat_char(in, out) is semidet.
%% :- mode seat_char(out, in) is det.
seat_char('.', no).
seat_char('L', yes(empty)).
seat_char('#', yes(occupied)).

input_seat_layout(Result, !IO) :-
    input_chars_lines(ReadResult, !IO),
    ( ReadResult = ok(Lines),
      ( map(map(seat_char), Lines, SeatsLists) ->
        Result = ok(from_lists(SeatsLists))
      ; Result = error(make_io_error("Invalid input"))
      )
    ; ReadResult = error(Error),
      Result = error(Error)
    ).
