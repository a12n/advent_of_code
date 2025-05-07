%% -*- mode: mercury -*-
:- module day_07.
:- interface.

:- import_module io, multi_map, pair.

:- type bag     == string.
:- type num_bag == pair(int, bag).

:- pred read_bags(res(multi_map(bag, num_bag))::out, io::di, io::uo) is det.
:- pred outermost(multi_map(bag, num_bag)::in, bag::in, bag::out) is nondet.
:- func contains_total(multi_map(bag, num_bag), bag) = int is semidet.

:- implementation.

:- import_module int, list, string.
:- import_module io_ext.

read_bags(Result, !IO) :-
    read_bags(init, Result, !IO).

:- pred read_bags( multi_map(bag, num_bag)::in,
                   res(multi_map(bag, num_bag))::out,
                   io::di, io::uo ) is det.
read_bags(Mapping0, Result, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      simplified(chomp(Line), String),
      ( bag_contains_string(String, Bag, Contains) ->
        foldl(add(Bag), Contains, Mapping0, Mapping),
        read_bags(Mapping, Result, !IO)
      ; Result = error(make_io_error("Invalid line"))
      )
    ; ReadResult = eof,
      Result = ok(Mapping0)
    ; ReadResult = error(Error),
      Result = error(Error)
    ).

:- pred simplified(string, string).
:- mode simplified(in, out) is det.
simplified(!String) :-
    replace_all(!.String, " bags contain ", ":", !:String),
    replace_all(!.String, " bags", "", !:String),
    replace_all(!.String, " bag", "", !:String),
    replace_all(!.String, "no other", "", !:String),
    replace_all(!.String, ".", "", !:String),
    replace_all(!.String, ", ", ",", !:String).

:- pred bag_contains_string(string::in, bag::out, list(num_bag)::out) is semidet.
bag_contains_string(String, Bag, Contains) :-
    [Bag, String1] = split_at_char(':', String),
    map(num_bag_string, negated_filter(is_empty, split_at_char(',', String1)), Contains).

:- pred num_bag_string(string::in, num_bag::out) is semidet.
num_bag_string(String, (N - Bag)) :-
    replace(String, " ", "=", String2),
    [NStr, Bag] = split_at_char('=', String2),
    to_int(NStr, N).

outermost(Mapping, Bag, OuterBag) :-
    member(Mapping, OtherBag, (_ - Bag)),
    ( OuterBag = OtherBag
    ; outermost(Mapping, OtherBag, OuterBag)
    ).

contains_total(Mapping, Bag) = Total :-
    search(Mapping, Bag, NumBags),
    foldl((pred((N - OtherBag)::in, Sum0::in, Sum::out) is det :-
               ( SubTotal = contains_total(Mapping, OtherBag) ->
                 Sum = Sum0 + N + N * SubTotal
               ; Sum = Sum0 + N
               )
          ), NumBags, 0, Total).
