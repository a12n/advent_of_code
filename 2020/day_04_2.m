%% -*- mode: mercury -*-
:- module day_04_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module io_ext.
:- import_module int, list, string, uint.

:- type year ---> year(int).
:- type height ---> centimeter(int); inch(int).
:- type color ---> color(uint).
:- type eye_color ---> amb; blu; brn; gry; grn; hzl; oth.
:- type passport_id ---> passport_id(string).
:- type country_id ---> country_id(string).
:- type field ---> byr(year); iyr(year); eyr(year); hgt(height);
   hcl(color); ecl(eye_color); pid(passport_id); cid(country_id).
:- type passport == list(field).

:- pred year_string(string, year).
:- mode year_string(in, out) is semidet.
year_string(String, year(Number)) :-
    length(String, 4),
    to_int(String, Number),
    Number >= 0, Number =< 9999.

:- pred height_string(string, height).
:- mode height_string(in, out) is semidet.
height_string(String, Height) :-
    ( append(Digits, "cm", String) ->
      to_int(Digits, Value), Value > 0,
      Height = centimeter(Value)
    ; append(Digits, "in", String) ->
      to_int(Digits, Value), Value > 0,
      Height = inch(Value)
    ; fail
    ).

:- pred color_string(string, color).
:- mode color_string(in, out) is semidet.
color_string(String, color(Bits)) :-
    append("#", Digits, String),
    length(Digits, 6),
    base_string_to_uint(16, Digits, Bits).

:- pred eye_color_string(string, eye_color).
:- mode eye_color_string(in, out) is semidet.
:- mode eye_color_string(out, in) is det.
eye_color_string("amb", amb).
eye_color_string("blu", blu).
eye_color_string("brn", brn).
eye_color_string("gry", gry).
eye_color_string("grn", grn).
eye_color_string("hzl", hzl).
eye_color_string("oth", oth).

:- pred passport_id_string(string, passport_id).
:- mode passport_id_string(in, out) is semidet.
passport_id_string(ID, passport_id(ID)) :-
    not is_empty(ID).

:- pred country_id_string(string, country_id).
:- mode country_id_string(in, out) is semidet.
country_id_string(ID, country_id(ID)) :-
    not is_empty(ID).

:- pred field_string(string, field).
:- mode field_string(in, out) is semidet.
field_string(String, Field) :-
    [Key, Value] = words_separator(unify(':'), String),
    field_key_value(Key, Value, Field).

:- pred field_key_value(string, string, field).
:- mode field_key_value(in, in, out) is semidet.
field_key_value("byr", Value, byr(Year)) :- year_string(Value, Year).
field_key_value("iyr", Value, iyr(Year)) :- year_string(Value, Year).
field_key_value("eyr", Value, eyr(Year)) :- year_string(Value, Year).
field_key_value("hgt", Value, hgt(Height)) :- height_string(Value, Height).
field_key_value("hcl", Value, hcl(Color)) :- color_string(Value, Color).
field_key_value("ecl", Value, ecl(EyeColor)) :- eye_color_string(Value, EyeColor).
field_key_value("pid", Value, pid(PassportID)) :- passport_id_string(Value, PassportID).
field_key_value("cid", Value, cid(CountryID)) :- country_id_string(Value, CountryID).

:- pred passport_string(string, passport).
:- mode passport_string(in, out) is semidet.
passport_string(String, Fields) :-
    map(field_string, words(String), Fields).

:- pred valid_field(field::in) is semidet.
valid_field(byr(year(Year))) :- Year >= 1920, Year =< 2002.
valid_field(iyr(year(Year))) :- Year >= 2010, Year =< 2020.
valid_field(eyr(year(Year))) :- Year >= 2020, Year =< 2030.
valid_field(hgt(centimeter(Height))) :- Height >= 150, Height =< 193.
valid_field(hgt(inch(Height))) :- Height >= 59, Height =< 76.
valid_field(hcl(_Valid)).
valid_field(ecl(_Valid)).
valid_field(pid(passport_id(ID))) :- length(ID, 9), is_all_digits(ID).
valid_field(cid(_Valid)).

:- pred valid_passport(passport::in) is semidet.
valid_passport(Fields) :-
    member(byr(_), Fields), member(iyr(_), Fields), member(eyr(_), Fields),
    member(hgt(_), Fields),
    member(hcl(_), Fields), member(ecl(_), Fields),
    member(pid(_), Fields),
    all_true(valid_field, Fields).

:- pred read_passport_string(result(string)::out, string::in, io::di, io::uo) is det.
read_passport_string(Result, Accum, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      String = chomp(Line),
      ( String = "" ->
        Result = ok(Accum)
      ; read_passport_string(Result, Accum ++ " " ++ String, !IO)
      )
    ; ReadResult = eof,
      ( Accum = "" ->
        Result = eof
      ; Result = ok(Accum)
      )
    ; ReadResult = error(Error),
      Result = error(Error)
    ).

:- pred main_loop(int::in, io::di, io::uo) is det.
main_loop(N, !IO) :-
    read_passport_string(ReadResult, "", !IO),
    ( ReadResult = ok(String),
      ( passport_string(String, Fields), valid_passport(Fields) ->
        main_loop(N + 1, !IO)
      ; main_loop(N, !IO)
      )
    ; ReadResult = eof,
      write_int(N, !IO), nl(!IO)
    ; ReadResult = error(Error),
      error_exit(1, error_message(Error), !IO)
    ).

main(!IO) :-
    main_loop(0, !IO).
