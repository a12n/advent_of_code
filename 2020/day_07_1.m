%% -*- mode: mercury -*-
:- module day_07_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, list, multi_map, solutions, string.
:- import_module io_ext.

:- pred contain(string, string).
:- mode contain(in, out) is nondet.
:- mode contain(out, in) is nondet.
contain("light red", "bright white").
contain("light red", "muted yellow").
contain("dark orange", "bright white").
contain("dark orange", "muted yellow").
contain("bright white", "shiny gold").
contain("muted yellow", "shiny gold").
contain("muted yellow", "faded blue").
contain("shiny gold", "dark olive").
contain("shiny gold", "vibrant plum").
contain("dark olive", "faded blue").
contain("dark olive", "dotted black").
contain("vibrant plum", "faded blue").
contain("vibrant plum", "dotted black").
%% contain("faded blue", _).
%% contain("dotted black", _).

:- pred eventually_contain(string, string).
:- mode eventually_contain(out, in) is nondet.
eventually_contain(X, Y) :-
    ( contain(X, Y)
    ; contain(Z, Y), eventually_contain(X, Z)
    ).

:- pred eventually_contain_shiny_gold(string).
:- mode eventually_contain_shiny_gold(out) is nondet.
eventually_contain_shiny_gold(Y) :-
    eventually_contain(Y, "shiny gold").

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
