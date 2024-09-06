-module(day_02).

-type shape() :: rock | paper | scissors.
-type round() :: {_Their :: shape(), _Your :: shape()}.
-type game() :: nonempty_list(round()).
-type outcome() :: lose | draw | win.

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    Game = read_game(standard_io),
    io:format(<<"~b~n">>, [game_score(Game)]);
main(2) ->
    %% TODO
    ok.

%%--------------------------------------------------------------------
%% Scoring functions.
%%--------------------------------------------------------------------

-spec game_score(game()) -> non_neg_integer().
game_score(Rounds) ->
    lists:sum(lists:map(fun round_score/1, Rounds)).

-spec round_score(round()) -> non_neg_integer().
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

-spec round_outcome(round()) -> outcome().
round_outcome({rock, scissors}) -> lose;
round_outcome({paper, rock}) -> lose;
round_outcome({scissors, paper}) -> lose;
round_outcome({rock, paper}) -> win;
round_outcome({paper, scissors}) -> win;
round_outcome({scissors, rock}) -> win;
round_outcome({Their, Your}) when Their == Your -> draw.

%%--------------------------------------------------------------------
%% Part 1 functions.
%%--------------------------------------------------------------------

-spec read_game(io:device()) -> game().
read_game(File) ->
    case io:get_line(File, <<>>) of
        eof -> [];
        <<Line/bytes>> -> [parse_round(string:trim(Line)) | read_game(File)]
    end.

-spec parse_round(binary()) -> round().
parse_round(<<Their, $\s, Your>>) ->
    {parse_shape(their, Their), parse_shape(your, Your)}.

-spec parse_shape(their | your, char()) -> shape().
parse_shape(their, $A) -> rock;
parse_shape(their, $B) -> paper;
parse_shape(their, $C) -> scissors;
parse_shape(your, $X) -> rock;
parse_shape(your, $Y) -> paper;
parse_shape(your, $Z) -> scissors.

%%--------------------------------------------------------------------
%% Part 2 functions.
%%--------------------------------------------------------------------
