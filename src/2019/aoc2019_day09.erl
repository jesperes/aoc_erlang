-module(aoc2019_day09).
 -behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 9,
                name = "Sensor Boost",
                expected = {2594708277, 87721},
                has_input_file = true}.

-type input_type() :: intcode:intcode_program().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    intcode:parse(Binary).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    {_, [Result]} = intcode:execute(Input, [1]),
    Result.

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    {_, [Result]} = intcode:execute(Input, [2]),
    Result.
