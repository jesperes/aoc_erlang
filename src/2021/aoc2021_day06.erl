-module(aoc2021_day06).

-behavior(aoc_puzzle).

-include_lib("eunit/include/eunit.hrl").

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 6,
                name = "Lanternfish",
                expected = {375482, 1689540415957},
                has_input_file = true}.

-type input_type() :: any().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    lists:foldl(fun(N, Map) -> maps:update_with(N, fun(Old) -> Old + 1 end, 1, Map) end,
                #{},
                lists:map(fun(<<N, _/binary>>) -> N - $0 end,
                          binary:split(Binary, <<",">>, [trim_all, global]))).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    num_fish(simulate(Input, 0, 80)).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    num_fish(simulate(Input, 0, 256)).

num_fish(Map) ->
    maps:fold(fun(_, V, Sum) -> Sum + V end, 0, Map).

simulate(Map, Max, Max) ->
    Map;

simulate(Map, N, Max) ->
    NewMap =
        maps:fold(fun (0, V, Acc) ->
                          maps:merge(#{6 => V, 8 => V}, Acc);
                      (Num, V, Acc) ->
                          maps:update_with(Num - 1, fun(Old) -> Old + V end, V, Acc)
                  end,
                  #{},
                  Map),
    simulate(NewMap, N + 1, Max).

ex1_test() ->
    ?assertEqual(26,
                 num_fish(simulate(#{3 => 2,
                                     4 => 1,
                                     1 => 1,
                                     2 => 1},
                                   0,
                                   18))).
