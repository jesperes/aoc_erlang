-module(aoc2019_day21).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 21,
                name = "Springdroid Adventure",
                expected = {19354437, 1145373084},
                has_input_file = true}.

-type input_type() :: intcode:intcode_program().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    intcode:parse(Binary).

%% I don't remember where I got the Springcode input from. Maybe I cheated and
%% stole the output from somewhere?

-spec solve1(Input :: input_type()) -> result_type().
solve1(Prog) ->
    Springcode = "OR A J\nAND B J\nAND C J\nNOT J J\nAND D J\nWALK\n",
    {_, [Damage | _]} = intcode:execute(Prog, Springcode),
    Damage.

-spec solve2(Input :: input_type()) -> result_type().
solve2(Prog) ->
    Springcode =
        "OR A J\nAND B J\nAND C J\nNOT J J\nAND D J\nOR E T\nOR H T\nAND "
        "T J\nRUN\n",
    {_, [Damage | _]} = intcode:execute(Prog, Springcode),
    Damage.
