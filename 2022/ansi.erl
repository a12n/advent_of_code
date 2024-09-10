-module(ansi).

-type color() ::
    black
    | red
    | green
    | yellow
    | blue
    | magenta
    | cyan
    | white.

-type attr() ::
    reset
    | bold
    | faint
    | italic
    | underline
    | {blink, slow | fast | false}
    | invert
    | strike
    | {fg | bg, color() | default}.

-type cursor() ::
    {position, {pos_integer(), pos_integer()}}
    | up
    | {up, pos_integer()}
    | down
    | {down, pos_integer()}
    | forward
    | {forward, pos_integer()}
    | back
    | {back, pos_integer()}.

-export_type([color/0, attr/0, cursor/0]).

-export([attrs/1, attrs/2, cursor/1]).

-define(CSI, "\e[").

-spec attrs(attr() | [attr() | iodata()]) -> iodata().
attrs(Attr) when is_atom(Attr); is_tuple(Attr) -> attrs([Attr]);
attrs(Attrs) when is_list(Attrs) ->
    [
        ?CSI,
        lists:join(
            $;,
            lists:map(
                fun
                    ([]) -> [];
                    (Bytes = <<_/bytes>>) -> Bytes;
                    (String = [_ | _]) -> String;
                    (Attr) -> attr_iodata(Attr)
                end,
                Attrs
            )
        ),
        $m
    ].

-spec attrs(attr() | [attr()], iodata()) -> iodata().
attrs(Attrs, Text) -> [attrs(Attrs), Text, attrs(reset)].

-spec cursor(cursor()) -> iodata().
cursor({position, {1, 1}}) ->
    [?CSI, $;, $H];
cursor({position, {Row, 1}}) when Row > 0 -> [?CSI, integer_to_binary(Row), $;, $H];
cursor({position, {1, Col}}) when Col > 0 -> [?CSI, $;, integer_to_binary(Col), $H];
cursor({position, {Row, Col}}) when Row > 0, Col > 0 ->
    [?CSI, integer_to_binary(Row), $;, integer_to_binary(Col), $H];
cursor(up) ->
    [?CSI, $A];
cursor(down) ->
    [?CSI, $B];
cursor(forward) ->
    [?CSI, $C];
cursor(back) ->
    [?CSI, $D];
cursor({up, 1}) ->
    cursor(up);
cursor({down, 1}) ->
    cursor(down);
cursor({forward, 1}) ->
    cursor(forward);
cursor({back, 1}) ->
    cursor(back);
cursor({up, Cells}) when Cells > 0 -> [?CSI, integer_to_binary(Cells), $A];
cursor({down, Cells}) when Cells > 0 -> [?CSI, integer_to_binary(Cells), $B];
cursor({forward, Cells}) when Cells > 0 -> [?CSI, integer_to_binary(Cells), $C];
cursor({back, Cells}) when Cells > 0 -> [?CSI, integer_to_binary(Cells), $D].

-spec attr_iodata(attr()) -> byte() | iodata().
attr_iodata(reset) -> $0;
attr_iodata(bold) -> $1;
attr_iodata(faint) -> $2;
attr_iodata(italic) -> $3;
attr_iodata(underline) -> $4;
attr_iodata({blink, slow}) -> $5;
attr_iodata({blink, fast}) -> $6;
attr_iodata({blink, false}) -> <<"25">>;
attr_iodata(invert) -> $7;
attr_iodata(strike) -> $9;
attr_iodata({fg, Color}) -> <<$3, (color_iodata(Color))>>;
attr_iodata({bg, Color}) -> <<$4, (color_iodata(Color))>>.

-spec color_iodata(color() | default) -> byte().
color_iodata(black) -> $0;
color_iodata(red) -> $1;
color_iodata(green) -> $2;
color_iodata(yellow) -> $3;
color_iodata(blue) -> $4;
color_iodata(magenta) -> $5;
color_iodata(cyan) -> $6;
color_iodata(white) -> $7;
color_iodata(default) -> $9.
