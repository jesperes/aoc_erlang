-module(aoc2018_day05).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2018,
                day = 5,
                name = "Alchemical Reduction",
                expected = {10496, 5774},
                has_input_file = true}.

-type input_type() :: string().
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    string:trim(binary_to_list(Input)).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    length(react(Input)).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(L) ->
    lists:min([solve1([C || C <- L, C =/= X, C =/= X - 32]) || X <- lists:seq($a, $z)]).

-spec react([integer()]) -> [integer()].
react(L) ->
    react(L, []).

-spec react(string(), string()) -> string().
react([], L) ->
    L;
react([C1 | L1], [C2 | L2]) when abs(C1 - C2) == 32 ->
    react(L1, L2);
react([C | L1], L2) ->
    react(L1, [C | L2]).
