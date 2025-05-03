%% -*- mode: mercury -*-
:- module day_04_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module io_ext.
:- import_module int, list, string.

:- type field_key ---> byr; iyr; eyr; hgt; hcl; ecl; pid; cid.

:- pred field_key_string(string, field_key).
:- mode field_key_string(out, in) is det.
:- mode field_key_string(in, out) is semidet.
field_key_string("byr", byr).
field_key_string("iyr", iyr).
field_key_string("eyr", eyr).
field_key_string("hgt", hgt).
field_key_string("hcl", hcl).
field_key_string("ecl", ecl).
field_key_string("pid", pid).
field_key_string("cid", cid).

:- pred field_string(string::in, field_key::out) is semidet.
field_string(String, Key) :-
    [KeyStr, _] = words_separator(unify(':'), String),
    field_key_string(KeyStr, Key).

:- pred field_list_string(string::in, list(field_key)::out) is semidet.
field_list_string(String, Fields) :-
    map(field_string, words(String), Fields).

:- func required_fields = list(field_key).
required_fields = [byr, iyr, eyr, hgt, hcl, ecl, pid].

:- pred main_loop(int::in, list(field_key)::in, io::di, io::uo) is det.
main_loop(N, MissingFields, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      String = chomp(Line),
      ( String = "" ->
        main_loop((MissingFields = [] -> N + 1; N), required_fields, !IO)
      ; ( field_list_string(String, Fields) ->
          main_loop(N, delete_elems(MissingFields, Fields), !IO)
        ; error_exit(1, "Invalid fields", !IO)
        )
      )
    ; ReadResult = eof,
      write_int((MissingFields = [] -> N + 1; N), !IO), nl(!IO)
    ; ReadResult = error(Error),
      error_exit(1, error_message(Error), !IO)
    ).

main(!IO) :-
    main_loop(0, required_fields, !IO).
