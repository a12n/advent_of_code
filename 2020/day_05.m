%% -*- mode: mercury -*-
:- module day_05.
:- interface.

:- type seat_id == uint.

:- pred seat_id_string(string, seat_id).
:- mode seat_id_string(in, out) is semidet.
%% :- mode seat_id_string(out, in) is det.

:- implementation.

:- import_module string.

seat_id_string(String0, ID) :-
    length(String0, 10),
    String1 = replace_all(String0, "F", "0"),
    String2 = replace_all(String1, "B", "1"),
    String3 = replace_all(String2, "L", "0"),
    String4 = replace_all(String3, "R", "1"),
    base_string_to_uint(2, String4, ID).
