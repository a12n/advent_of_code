-module(io_ext).

-export([fread_lines/2, fread_lines/3, read_lines/1, read_lines/2]).

-spec read_lines(io:device()) -> [binary()].
read_lines(File) -> read_lines(File, -1).

-spec read_lines(io:device(), integer()) -> [binary()].
read_lines(_File, 0) ->
    [];
read_lines(File, N) ->
    case io:get_line(File, <<>>) of
        eof ->
            [];
        <<Line/bytes>> ->
            %% Trim the trailing \n.
            [<<Line:(byte_size(Line) - 1)/bytes>> | read_lines(File, N - 1)];
        Line when is_list(Line) ->
            %% Trim the trailing \n in a string.
            [lists:droplast(Line) | read_lines(File, N - 1)]
    end.

-spec fread_lines(io:device(), string() | binary()) -> [[term()]].
fread_lines(File, Format) ->
    fread_lines(File, Format, -1).

%% XXX
-spec fread_lines(io:device(), string() | binary(), integer()) -> [[term()]].
fread_lines(_, _, 0) ->
    [];
fread_lines(File, Format, N) ->
    case io:fread(File, <<>>, Format) of
        eof -> [];
        {ok, Terms} -> [Terms | fread_lines(File, Format, N - 1)]
    end.
