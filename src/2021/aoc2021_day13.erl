-module(aoc2021_day13).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 13,
                name = "Transparent Origami",
                expected =
                    {701,
                     "####.###..####.#..#.###..####...##.#...\n"
                     ++ "#....#..#.#....#.#..#..#.#.......#.#...\n"
                     ++ "###..#..#.###..##...###..###.....#.#...\n"
                     ++ "#....###..#....#.#..#..#.#.......#.#...\n"
                     ++ "#....#....#....#.#..#..#.#....#..#.#...\n"
                     ++ "#....#....####.#..#.###..####..##..####\n"},
                has_input_file = true}.

-type input_type() :: {sets:set(), [{atom(), integer()}]}.
-type result1_type() :: integer().
-type result2_type() :: string().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    {Coords, Folds} =
        lists:foldl(fun(B, {Coords, Folds}) ->
                       case string:tokens(binary_to_list(B), ",= ") of
                           [X, Y] ->
                               {sets:add_element({list_to_integer(X), list_to_integer(Y)}, Coords),
                                Folds};
                           ["fold", "along", Dir, Num] ->
                               {Coords, [{list_to_atom(Dir), list_to_integer(Num)} | Folds]}
                       end
                    end,
                    {sets:new(), []},
                    binary:split(Binary, <<"\n">>, [trim_all, global])),
    {Coords, lists:reverse(Folds)}.

-spec solve1(Input :: input_type()) -> result1_type().
solve1({Coords, [{Dir, Num} | _] = _Folds}) ->
    sets:size(fold(Dir, Num, Coords)).

-spec solve2(Input :: input_type()) -> result2_type().
solve2({Coords, Folds}) ->
    FoldedCoords =
        lists:foldl(fun({Dir, Num}, Acc) -> fold(Dir, Num, Acc) end, Coords, Folds),
    set_to_str(FoldedCoords).

fold(Dir, Num, Coords) ->
    sets:fold(fun ({X, Y}, Acc) when Dir =:= y andalso Y > Num ->
                      %% horizontal fold
                      sets:add_element({X, 2 * Num - Y}, Acc);
                  ({X, Y}, Acc) when Dir =:= x andalso X > Num ->
                      %% vertical fold
                      sets:add_element({2 * Num - X, Y}, Acc);
                  (Coord, Acc) ->
                      %% above/left of fold, keep as-is
                      sets:add_element(Coord, Acc)
              end,
              sets:new(),
              Coords).

set_to_str(Set) ->
    Coords = sets:to_list(Set),
    XMax =
        lists:max(
            lists:map(fun({X, _}) -> X end, Coords)),
    YMax =
        lists:max(
            lists:map(fun({_, Y}) -> Y end, Coords)),

    Str = [[case sets:is_element({X, Y}, Set) of
                true ->
                    $#;
                false ->
                    $.
            end
            || X <- lists:seq(0, XMax)]
           ++ "\n"
           || Y <- lists:seq(0, YMax)],
    lists:flatten(Str).

%% Tests
-ifdef(TEST).

ex1_test() ->
    Binary =
        <<"6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n"
          "3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along "
          "x=5\n">>,
    {Coords, _} = parse(Binary),
    Coords0 = fold(y, 7, Coords),
    Coords1 = fold(x, 5, Coords0),
    ?debugFmt("~n~s", [set_to_str(Coords1)]),
    ?assertEqual(17, sets:size(Coords1)).

-endif.
