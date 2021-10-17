-module(aoc2019_day02).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 2,
                name = "1202 Program Alarm",
                expected = {3654868, 7014},
                has_input_file = true}.

-type input_type() :: intcode:intcode_program().
-type result_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    intcode:parse(Input).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    run_intcode(Input, 12, 2).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    [Result] =
        [100 * A + B
         || A <- lists:seq(0, 99), B <- lists:seq(0, 99), run_intcode(Input, A, B) =:= 19690720],
    Result.

run_intcode(Prog, A, B) ->
    {ProgOut, _} =
        intcode:execute(
            maps:merge(Prog, #{1 => A, 2 => B})),
    maps:get(0, ProgOut).
