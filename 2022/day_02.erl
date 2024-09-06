-module(day_02).

-type outcome() :: lose | draw | win.
-type shape() :: rock | paper | scissors.

-type round_played() :: {_Their :: shape(), _Your :: shape()}.

-type game() :: nonempty_list(round_played()).

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    Game = lists:map(fun parse_round_played/1, file_lines(standard_io)),
    io:format(<<"~b~n">>, [game_score(Game)]);
main(2) ->
    %% TODO
    ok.

%%--------------------------------------------------------------------
%% Game logic functions.
%%--------------------------------------------------------------------

%% What shape the specified shape defeats.
-spec defeats(shape()) -> shape().
defeats(rock) -> scissors;
defeats(paper) -> rock;
defeats(scissors) -> paper.

%% To what shape the specified shape losses.
-spec looses(shape()) -> shape().
looses(rock) -> paper;
looses(paper) -> scissors;
looses(scissors) -> rock.

%%--------------------------------------------------------------------
%% Scoring functions.
%%--------------------------------------------------------------------

-spec game_score(game()) -> non_neg_integer().
game_score(Rounds) ->
    lists:sum(lists:map(fun round_score/1, Rounds)).

-spec round_score(round_played()) -> non_neg_integer().
round_score(Round = {_Their, Your}) ->
    selected_shape_score(Your) + outcome_score(round_outcome(Round)).

-spec selected_shape_score(shape()) -> non_neg_integer().
selected_shape_score(rock) -> 1;
selected_shape_score(paper) -> 2;
selected_shape_score(scissors) -> 3.

-spec outcome_score(outcome()) -> non_neg_integer().
outcome_score(lose) -> 0;
outcome_score(draw) -> 3;
outcome_score(win) -> 6.

-spec round_outcome(round_played()) -> outcome().
round_outcome({rock, scissors}) -> lose;
round_outcome({paper, rock}) -> lose;
round_outcome({scissors, paper}) -> lose;
round_outcome({rock, paper}) -> win;
round_outcome({paper, scissors}) -> win;
round_outcome({scissors, rock}) -> win;
round_outcome({Their, Your}) when Their == Your -> draw.

%%--------------------------------------------------------------------
%% Parsing functions.
%%--------------------------------------------------------------------

-spec file_lines(io:device()) -> [binary()].
file_lines(File) ->
    case io:get_line(File, <<>>) of
        eof -> [];
        <<Line/bytes>> -> [string:trim(Line) | file_lines(File)]
    end.

-spec parse_shape(char()) -> shape().
parse_shape($A) -> rock;
parse_shape($B) -> paper;
parse_shape($C) -> scissors.

%%--------------------------------------------------------------------
%% Part 1 functions.
%%--------------------------------------------------------------------

-spec parse_round_played(binary()) -> round_played().
parse_round_played(<<Their, $\s, Your>>) ->
    {
        parse_shape(Their),
        parse_shape(
            case Your of
                $X -> $A;
                $Y -> $B;
                $Z -> $C
            end
        )
    }.

%%--------------------------------------------------------------------
%% Part 2 functions.
%%--------------------------------------------------------------------
