%% -*- mode: mercury -*-
:- module day_02.
:- interface.

:- import_module char, int, io, string.

:- pred input_line(string::in, policy::out, password::out) is semidet.
:- pred write_num_valid(int::in, check_pred::in, io::di, io::uo) is cc_multi.

:- type check_pred ---> check(pred(policy::in, password::in) is semidet).
:- type password ---> password(string).
:- type policy --->	policy(min_times :: int, max_times :: int, letter :: char).

:- implementation.

:- import_module list.
:- import_module io_ext.

write_num_valid(N, check(Valid), !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      ( input_line(chomp(Line), Policy, Password) ->
        write_num_valid((Valid(Policy, Password) -> N + 1; N), check(Valid), !IO)
      ; error_exit(1, "invalid input", !IO)
      )
    ; ReadResult = eof,
      write_int(N, !IO),
      nl(!IO)
    ; ReadResult = error(Error),
      error_exit(1, error_message(Error), !IO)
    ).

input_line(String, Policy, Password) :-
    [PolicyPart, PasswordString] = words_separator(unify(':'), String),
    [RangePart, LetterString] = words(PolicyPart),
    [MinString, MaxString] = words_separator(unify('-'), RangePart),
    to_int(MinString, MinTimes),
    to_int(MaxString, MaxTimes),
    Letter = LetterString ^ elem(0),
    Policy = policy(MinTimes, MaxTimes, Letter),
    Password = password(strip(PasswordString)).
