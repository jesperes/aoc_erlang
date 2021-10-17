-module(aoc2019_day05).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 5,
                name = "Sunny with a Chance of Asteroids",
                expected = {16348437, 6959377},
                has_input_file = true}.

-type input_type() :: intcode:intcode_program().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    intcode:parse(Binary).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    execute(Input, 1).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    execute(Input, 5).

execute(Prog, Input) ->
    {_, [Output | _]} = intcode:execute(Prog, [Input]),
    Output.
