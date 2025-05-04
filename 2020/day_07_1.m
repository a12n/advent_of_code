%% -*- mode: mercury -*-
:- module day_07_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module solutions.

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

main(!IO) :-
    solutions(eventually_contain_shiny_gold, Bags),
    print_line(Bags, !IO).
