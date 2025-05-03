%% -*- mode: mercury -*-
:- module day_05_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type seat_id == uint.

:- import_module string, uint.
:- import_module io_ext.

:- pred seat_id_string(string, seat_id).
:- mode seat_id_string(in, out) is semidet.
%% :- mode seat_id_string(out, in) is det.
seat_id_string(String0, ID) :-
    length(String0, 10),
    String1 = replace_all(String0, "F", "0"),
    String2 = replace_all(String1, "B", "1"),
    String3 = replace_all(String2, "L", "0"),
    String4 = replace_all(String3, "R", "1"),
    base_string_to_uint(2, String4, ID).

:- pred main_loop(seat_id::in, io::di, io::uo) is det.
main_loop(MaxID, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      String = chomp(Line),
      ( seat_id_string(String, ID) ->
        main_loop(max(MaxID, ID), !IO)
      ; error_exit(1, "Invalid boarding pass", !IO)
      )
    ; ReadResult = eof,
      write_uint(MaxID, !IO), nl(!IO)
    ; ReadResult = error(Error),
      error_exit(1, error_message(Error), !IO)
    ).

main(!IO) :-
    main_loop(0u, !IO).
