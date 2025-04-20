%% -*- mode: mercury -*-
:- module day_02_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module char, int, list, string.
:- import_module io_ext, string_ext.

:- type password ---> password(string).
:- type policy --->	policy(min_times :: int, max_times :: int, letter :: char).

main(!IO) :-
    main(0, !IO).

:- pred main(int::in, io::di, io::uo) is cc_multi.
main(N, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      ( input_line(chomp(Line), Policy, Password) ->
        main((valid(Policy, Password) -> N + 1; N), !IO)
      ; error_exit(1, "invalid input", !IO)
      )
    ; ReadResult = eof,
      write_int(N, !IO),
      nl(!IO)
    ; ReadResult = error(Error),
      error_exit(1, error_message(Error), !IO)
    ).

:- pred input_line(string::in, policy::out, password::out) is semidet.
input_line(String, Policy, Password) :-
    [PolicyPart, PasswordString] = words_separator(unify(':'), String),
    [RangePart, LetterString] = words(PolicyPart),
    [MinString, MaxString] = words_separator(unify('-'), RangePart),
    to_int(MinString, MinTimes),
    to_int(MaxString, MaxTimes),
    Letter = LetterString ^ elem(0),
    Policy = policy(MinTimes, MaxTimes, Letter),
    Password = password(strip(PasswordString)).

:- pred valid(policy::in, password::in) is semidet.
valid(policy(MinTimes, MaxTimes, Letter), password(Password)) :-
    contains_char(Password, Letter, Times),
    Times >= MinTimes,
    Times =< MaxTimes.
