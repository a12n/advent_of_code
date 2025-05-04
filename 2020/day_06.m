%% -*- mode: mercury -*-
:- module day_06.
:- interface.

:- import_module char, string.

%% Only 26 bits are needed to represent the answers.
:- type answer_set == uint32.

:- pred char_answer(char::in, answer_set::out) is semidet.
:- pred char_answer(char::in, answer_set::in, answer_set::out) is semidet.

:- pred string_answers(string::in, answer_set::out) is semidet.

:- implementation.

:- import_module int, uint32.

char_answer(Char, Answer) :-
    char_answer(Char, 0u32, Answer).

char_answer(Char, Accum, Answer) :-
    is_alpha(Char),
    N = to_int(to_lower(Char)) - to_int('a'),
    Answer = Accum \/ (1u32 << N).

string_answers(String, Answers) :-
    foldl(char_answer, String, 0u32, Answers).
