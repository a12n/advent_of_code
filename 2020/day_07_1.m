%% -*- mode: mercury -*-
:- module day_07_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, multi_map, solutions, string.
:- import_module io_ext.

:- pred simplified(string, string).
:- mode simplified(in, out) is det.
simplified(!String) :-
    replace_all(!.String, " bags contain ", ":", !:String),
    replace_all(!.String, " bags", "", !:String),
    replace_all(!.String, " bag", "", !:String),
    replace_all(!.String, "no other", "", !:String),
    replace_all(!.String, ".", "", !:String),
    replace_all(!.String, ", ", ",", !:String).

:- pred contain_string(string::in, string::out, list(string)::out) is semidet.
contain_string(String, Bag, Contains) :-
    [Bag, String1] = split_at_char(':', String),
    map((pred(String2::in, ChildBag::out) is semidet :-
             replace(String2, " ", "=", String3),
             [_, ChildBag] = split_at_char('=', String3)
        ),
        negated_filter(is_empty, split_at_char(',', String1)),
        Contains
       ).

:- pred outermost(multi_map(string, string)::in, string::in, string::out) is nondet.
outermost(Mapping, Bag, OuterBag) :-
    member(Mapping, OtherBag, Bag),
    ( OuterBag = OtherBag
    ; outermost(Mapping, OtherBag, OuterBag)
    ).

:- pred contain(string::in, multi_map(string, string)::in, multi_map(string, string)::out) is semidet.
contain(String0, !Mapping) :-
    simplified(chomp(String0), String),
    contain_string(String, Bag, ContainBags),
    foldl(add(Bag), ContainBags, !Mapping).

main(!IO) :-
    lines_foldl(contain, init, Result, !IO),
    ( Result = ok(Mapping),
      solutions(outermost(Mapping, "shiny gold"), Solutions),
      write_int(length(Solutions), !IO), nl(!IO)
    ; Result = error(_, Error),
      error_exit(1, error_message(Error), !IO)
    ).
