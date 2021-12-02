-module(aoc2021_day01).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 1,
                name = "Sonar Sweep",
                expected = {1400, 1429},
                has_input_file = true}.

-type input_type() :: [pos_integer()].
-type result_type() :: pos_integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    lists:map(fun(B) -> binary_to_integer(B) end,
              binary:split(Binary, <<"\n">>, [trim_all, global])).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    count_sliding(Input, 0).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    count_sliding3(Input, 0).

count_sliding([_], Acc) ->
    Acc;
count_sliding([A | [B | _] = Rest], Acc) when B > A ->
    count_sliding(Rest, Acc + 1);
count_sliding([_ | Rest], Acc) ->
    count_sliding(Rest, Acc).

count_sliding3([_, _, _], Acc) ->
    Acc;
count_sliding3([A1 | [AB2, AB3, B4 | _] = Rest], Acc)
    when AB2 + AB3 + B4 > A1 + AB2 + AB3 ->
    count_sliding3(Rest, Acc + 1);
count_sliding3([_ | Rest], Acc) ->
    count_sliding3(Rest, Acc).
