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

-type input_type() :: {map(), [{atom(), integer()}]}.
-type result1_type() :: integer().
-type result2_type() :: string().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    {Coords, Folds} =
        lists:foldl(fun(B, {Coords, Folds}) ->
                       case string:tokens(binary_to_list(B), ",= ") of
                           [X, Y] ->
                               {maps:put({list_to_integer(X), list_to_integer(Y)}, true, Coords),
                                Folds};
                           ["fold", "along", Dir, Num] ->
                               {Coords, [{list_to_atom(Dir), list_to_integer(Num)} | Folds]}
                       end
                    end,
                    {#{}, []},
                    binary:split(Binary, <<"\n">>, [trim_all, global])),
    {Coords, lists:reverse(Folds)}.

-spec solve1(Input :: input_type()) -> result1_type().
solve1({Coords, [First | _] = _Folds}) ->
    maps:size(fold(First, Coords)).

-spec solve2(Input :: input_type()) -> result2_type().
solve2({Coords, Folds}) ->
    to_str(lists:foldl(fun fold/2, Coords, Folds)).

fold({x, Num}, Coords) ->
    maps:fold(fun ({X, Y}, _, Acc) when X > Num ->
                      maps:put({2 * Num - X, Y}, true, Acc);
                  (Coord, _, Acc) ->
                      maps:put(Coord, true, Acc)
              end,
              #{},
              Coords);
fold({y, Num}, Coords) ->
    maps:fold(fun ({X, Y}, _, Acc) when Y > Num ->
                      maps:put({X, 2 * Num - Y}, true, Acc);
                  (Coord, _, Acc) ->
                      maps:put(Coord, true, Acc)
              end,
              #{},
              Coords).

to_str(Set) ->
    Coords = maps:keys(Set),
    XMax =
        lists:max(
            lists:map(fun({X, _}) -> X end, Coords)),
    YMax =
        lists:max(
            lists:map(fun({_, Y}) -> Y end, Coords)),

    Str = [[case maps:is_key({X, Y}, Set) of
                true ->
                    $#;
                false ->
                    $.
            end
            || X <- lists:seq(0, XMax)]
           ++ "\n"
           || Y <- lists:seq(0, YMax)],
    lists:flatten(Str).
