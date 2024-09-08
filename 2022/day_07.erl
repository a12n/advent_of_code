-module(day_07).

-include_lib("eunit/include/eunit.hrl").

-record(file_entry, {
    size :: non_neg_integer()
}).

-record(dir_entry, {
    size :: non_neg_integer(),
    children = #{} :: #{binary() := #dir_entry{} | #file_entry{}}
}).

-export([main/1]).

-spec main(1..2) -> ok.
main(_Part) ->
    Root = build_tree(io_ext:read_lines(standard_io)),
    ?debugVal(Root),
    Root2 = update_size(Root),
    ?debugVal(Root2),
    Found = find_dirs(Root2),
    ?debugVal(Found),
    Size = lists:foldl(fun(#dir_entry{size = Size}, Total) -> Total + Size end, 0, Found),
    io:format(<<"~b~n">>, [Size]).

-spec build_tree([binary()]) -> #dir_entry{}.
build_tree([<<"$ cd /">> | Commands]) ->
    {[], Root} = build_tree(Commands, #dir_entry{}),
    Root.

-spec build_tree([binary()], #dir_entry{}) -> {[binary()], #dir_entry{}}.
build_tree([], Cur) ->
    ?debugHere,
    {[], Cur};
build_tree([<<"$ cd ..">> | Commands], Cur) ->
    ?debugFmt("cd .., cur ~p~n", [Cur]),
    {Commands, Cur};
build_tree([<<"$ cd ", Name/bytes>> | Commands], Cur = #dir_entry{children = Children}) ->
    ?debugFmt("cd ~p, cur ~p~n", [Name, Cur]),
    {Commands2, Child} = build_tree(Commands, maps:get(Name, Children, #dir_entry{})),
    Children2 = maps:update_with(Name, fun(_) -> Child end, Child, Children),
    Cur2 = Cur#dir_entry{children = Children2},
    ?debugFmt("after cd ~p, cur ~p~n", [Name, Cur2]),
    build_tree(Commands2, Cur2);
build_tree([<<"$ ls">> | Commands], Cur) ->
    ?debugFmt("ls, cur ~p~n", [Cur]),
    build_tree(Commands, Cur);
build_tree([<<"dir ", Name/bytes>> | Commands], Cur = #dir_entry{children = Children}) ->
    Children2 = maps:update_with(
        Name, fun(_) -> error(name_exists) end, #dir_entry{}, Children
    ),
    Cur2 = Cur#dir_entry{children = Children2},
    ?debugFmt("dir ~p, cur ~p~n", [Name, Cur2]),
    build_tree(Commands, Cur2);
build_tree([FileSize | Commands], Cur = #dir_entry{children = Children}) ->
    [SizeStr, Name] = binary:split(FileSize, <<" ">>, [global, trim]),
    Size = binary_to_integer(SizeStr),
    Children2 = maps:update_with(
        Name, fun(_) -> error(name_exists) end, #file_entry{size = Size}, Children
    ),
    Cur2 = Cur#dir_entry{children = Children2},
    ?debugFmt("file ~p ~p, cur ~p~n", [Name, Size, Cur2]),
    build_tree(Commands, Cur2).

-spec update_size(#dir_entry{}) -> #dir_entry{}.
update_size(Dir = #dir_entry{size = undefined, children = Children}) ->
    Children2 = maps:map(
        fun
            (_Name, File = #file_entry{}) -> File;
            (_Name, SubDir = #dir_entry{}) -> update_size(SubDir)
        end,
        Children
    ),
    Size = maps:fold(
        fun
            (_Name, #file_entry{size = Size}, Total) -> Total + Size;
            (_Name, #dir_entry{size = Size}, Total) -> Total + Size
        end,
        0,
        Children2
    ),
    Dir#dir_entry{size = Size, children = Children2};
update_size(Dir = #dir_entry{size = Size}) when is_integer(Size) -> Dir.

-spec find_dirs(#dir_entry{}|#file_entry{}) -> [#dir_entry{}].
find_dirs(#file_entry{}) -> [];
find_dirs(Dir=#dir_entry{size = Size,children = Children}) ->
    SubDirs = lists:flatten(lists:map(fun find_dirs/1, maps:values(Children))),
    case Size =< 100000 of
        true -> [Dir|SubDirs];
        false ->SubDirs
    end.
