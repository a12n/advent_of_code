%% -*- mode: mercury -*-
:- module day_13.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pair.

:- pred input(res(pair(int, list(maybe(int))))::out, io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module string.

input(Result, !IO) :-
    read_line_as_string(ReadTime, !IO),
    read_line_as_string(ReadBuses, !IO),
    ( ReadTime = ok(TimeLine), ReadBuses = ok(BusesLine) ->
      ( to_int(chomp(TimeLine), Time),
        map(bus_string, split_at_char(',', chomp(BusesLine)), Buses) ->
        Result = ok(Time - Buses)
      ; Result = error(make_io_error("Invalid input"))
      )
    ; ReadTime = error(Error) -> Result = error(Error)
    ; ReadBuses = error(Error) -> Result = error(Error)
    ; Result = error(make_io_error("Unexpected EOF"))
    ).

:- pred bus_string(string::in, maybe(int)::out) is semidet.
bus_string(String, Bus) :-
    ( String = "x" -> Bus = no
    ; to_int(String, ID), Bus = yes(ID)
    ).
