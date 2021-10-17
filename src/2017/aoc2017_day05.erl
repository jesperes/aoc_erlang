-module(aoc2017_day05).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 5,
                name = "A Maze of Twisty Trampolines, All Alike",
                expected = {339351, 24315397},
                has_input_file = true}.

-type input_type() :: map().
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    to_map(lists:map(fun list_to_integer/1, string:tokens(binary_to_list(Input), "\n\r"))).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    execute(Input, 0, 0, fun(N) -> N + 1 end).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    execute(Input,
            0,
            0,
            fun (N) when N >= 3 ->
                    N - 1;
                (N) ->
                    N + 1
            end).

execute(Offsets, PC, Steps, Fun) ->
    case maps:get(PC, Offsets, eop) of
        eop ->
            Steps;
        N ->
            execute(maps:update_with(PC, Fun, Offsets), PC + N, Steps + 1, Fun)
    end.

to_map(Ints) ->
    Indexes = lists:seq(0, length(Ints) - 1),
    maps:from_list(
        lists:zip(Indexes, Ints)).
