-module(day_07).

-include_lib("eunit/include/eunit.hrl").

-record(file_entry, {
    size :: non_neg_integer()
}).

-record(dir_entry, {
    size :: non_neg_integer(),
    children = #{} :: #{binary() := #dir_entry{} | #file_entry{}}
}).

-type file_entry() :: #file_entry{}.
-type dir_entry() :: #dir_entry{}.
-type entry() :: dir_entry() | file_entry().

-export([main/1]).

-spec main(1..2) -> ok.
main(Part) ->
    Root = update_size(build_tree(io_ext:read_lines(standard_io))),
    case Part of
        1 ->
            SmallerDirSizes = filtermap_entries(
                fun
                    (#dir_entry{size = Size}) when Size =< 100000 -> {true, Size};
                    (_Other) -> false
                end,
                Root
            ),
            io:format(<<"~b~n">>, [lists:sum(SmallerDirSizes)]);
        2 ->
            SpaceUsed = Root#dir_entry.size,
            SpaceAvail = 70000000 - SpaceUsed,
            SpaceNeeded = 30000000 - SpaceAvail,
            DirSizes = lists:sort(
                filtermap_entries(
                    fun
                        (#dir_entry{size = Size}) -> {true, Size};
                        (_File) -> false
                    end,
                    Root
                )
            ),
            [Size | _] = lists:dropwhile(fun(Size) -> Size < SpaceNeeded end, DirSizes),
            io:format(<<"~b~n">>, [Size])
    end.

-spec build_tree([binary()]) -> #dir_entry{}.
build_tree([<<"$ cd /">> | Commands]) ->
    {[], Root} = build_tree(Commands, #dir_entry{}),
    Root.

-spec build_tree([binary()], #dir_entry{}) -> {[binary()], #dir_entry{}}.
build_tree([], Cur) ->
    {[], Cur};
build_tree([<<"$ cd ..">> | Commands], Cur) ->
    {Commands, Cur};
build_tree([<<"$ cd ", Name/bytes>> | Commands], Cur = #dir_entry{children = Children}) ->
    {Commands2, Child} = build_tree(Commands, maps:get(Name, Children, #dir_entry{})),
    Children2 = maps:update_with(Name, fun(_) -> Child end, Child, Children),
    build_tree(Commands2, Cur#dir_entry{children = Children2});
build_tree([<<"$ ls">> | Commands], Cur) ->
    build_tree(Commands, Cur);
build_tree([<<"dir ", Name/bytes>> | Commands], Cur = #dir_entry{children = Children}) ->
    Children2 = maps:update_with(
        Name, fun(_) -> error(name_exists) end, #dir_entry{}, Children
    ),
    build_tree(Commands, Cur#dir_entry{children = Children2});
build_tree([FileSize | Commands], Cur = #dir_entry{children = Children}) ->
    [SizeStr, Name] = binary:split(FileSize, <<" ">>, [global, trim]),
    Size = binary_to_integer(SizeStr),
    Children2 = maps:update_with(
        Name, fun(_) -> error(name_exists) end, #file_entry{size = Size}, Children
    ),
    build_tree(Commands, Cur#dir_entry{children = Children2}).

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

-spec filtermap_entries(fun((entry()) -> boolean() | {true, term()}), entry()) -> [term()].
filtermap_entries(Pred, File = #file_entry{}) ->
    case Pred(File) of
        {true, Value} -> [Value];
        true -> [File];
        false -> []
    end;
filtermap_entries(Pred, Dir = #dir_entry{children = Children}) ->
    SubEntries = lists:flatten(
        lists:map(
            fun(Entry) ->
                filtermap_entries(Pred, Entry)
            end,
            maps:values(Children)
        )
    ),
    case Pred(Dir) of
        {true, Value} -> [Value | SubEntries];
        true -> [Dir | SubEntries];
        false -> SubEntries
    end.
