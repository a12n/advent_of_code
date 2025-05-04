%% -*- mode: mercury -*-
:- module day_07_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, list, solutions, string.
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

:- pred preprocessed(string, string).
:- mode preprocessed(in, out) is det.
preprocessed(String0, String) :-
    String1 = replace_all(String0, " bags contain ", ":"),
    String2 = replace_all(String1, " bags", ""),
    String3 = replace_all(String2, " bag", ""),
    String4 = replace_all(String3, "no other", ""),
    String5 = replace_all(String4, ".", ""),
    String = replace_all(String5, ", ", ",").

:- pred contain_string(string::in, string::out, list({int, string})::out) is semidet.
contain_string(String, Bag, ContainBags) :-
    [Bag, String1] = split_at_char(':', String),
    map((pred(String2::in, {N, ChildBag}::out) is semidet :-
             replace(String2, " ", "=", String3),
             [NumString, ChildBag] = split_at_char('=', String3),
             to_int(NumString, N)
        ),
        negated_filter(is_empty, split_at_char(',', String1)),
        ContainBags
       ).

main(!IO) :-
    %% %% solutions(eventually_contain_shiny_gold, Bags),
    %% %% print_line(Bags, !IO).
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      String0 = chomp(Line),
      preprocessed(String0, String1),
      format("\"%s\" -> \"%s\"\n", [s(String0), s(String1)], !IO),
      ( contain_string(String1, Bag, ContainBags) ->
        format("Bag \"%s\" ", [s(Bag)], !IO),
        print_line(ContainBags, !IO)
      ; print_line("Couldn't parse string", !IO)
      ),
      %% ( [Bag, Other] = split_at_string(" bags contain ", String0) ->
      %%   format("Bag \"%s\", Other \"%s\"\n", [s(Bag), s(Other)], !IO)
      %% ; format("Couldn't split string \"%s\"\n", [s(String0)], !IO)
      %% ),
      main(!IO)
    ; ReadResult = eof
    ; ReadResult = error(Error),
      error_exit(1, error_message(Error), !IO)
    ).
