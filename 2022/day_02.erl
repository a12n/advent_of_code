-module(day_02).

-type shape() :: rock | paper | scissors.
-type round() :: {_Their :: shape(), _Your :: shape()}.
-type game() :: nonempty_list(round()).

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    Game = read_game(standard_io),
    io:format(<<"~b~n">>, [game_score(Game)]).

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------

-spec read_game(io:device()) -> game().
read_game(File) ->
    case io:get_line(File, <<>>) of
        eof -> [];
        <<Line/bytes>> -> [parse_round(string:trim(Line)) | read_game(File)]
    end.

-spec game_score(game()) -> non_neg_integer().
game_score(Rounds) ->
    lists:sum(lists:map(fun round_score/1, Rounds)).

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

-spec round_score(round()) -> non_neg_integer().
round_score(Round = {_Their, Your}) ->
    selected_shape_score(Your) + round_outcome_score(Round).

-spec selected_shape_score(shape()) -> non_neg_integer().
selected_shape_score(rock) -> 1;
selected_shape_score(paper) -> 2;
selected_shape_score(scissors) -> 3.

-spec round_outcome_score(round()) -> non_neg_integer().
%% You lost.
round_outcome_score({rock, scissors}) -> 0;
round_outcome_score({paper, rock}) -> 0;
round_outcome_score({scissors, paper}) -> 0;
%% You won.
round_outcome_score({rock, paper}) -> 6;
round_outcome_score({paper, scissors}) -> 6;
round_outcome_score({scissors, rock}) -> 6;
%% Was a draw.
round_outcome_score({Their, Your}) when Their == Your -> 3.
