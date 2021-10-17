-module(aoc2019_day01).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 1,
                name = "The Tyranny of the Rocket Equation",
                expected = {3368364, 5049684},
                has_input_file = true}.

-type input_type() :: [integer()].
-type result_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    lists:map(fun erlang:list_to_integer/1, string:tokens(binary_to_list(Input), "\n\r")).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    lists:foldl(fun(M, A) -> A + (M div 3 - 2) end, 0, Input).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    lists:foldl(fun(M, A) -> A + fuel(M) end, 0, Input).

-spec fuel(integer()) -> integer().
fuel(M) ->
    F = M div 3 - 2,
    case F =< 0 of
        true ->
            0;
        false ->
            F + fuel(F)
    end.
