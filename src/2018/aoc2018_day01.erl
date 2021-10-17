-module(aoc2018_day01).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2018,
                day = 1,
                name = "Chronal Calibration",
                expected = {470, 790},
                has_input_file = true}.

-type input_type() :: [integer()].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    lists:map(fun list_to_integer/1, string:tokens(binary_to_list(Input), "\n\r")).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    lists:foldl(fun(A, B) -> A + B end, 0, Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    find_first_duplicate(Input).

%%% Implementation

find_first_duplicate(IntList) ->
    find_first_duplicate_repeat(IntList, 0, sets:from_list([0])).

find_first_duplicate_repeat(IntList, Freq, Set) ->
    case find_first_duplicate(IntList, Freq, Set) of
        {true, NewFreq} ->
            NewFreq;
        {false, LastFreq, LastSet} ->
            find_first_duplicate_repeat(IntList, LastFreq, LastSet)
    end.

find_first_duplicate([], Freq, FreqSet) ->
    {false, Freq, FreqSet};
find_first_duplicate([N | IntList], Freq, FreqSet) ->
    NewFreq = N + Freq,
    case sets:is_element(NewFreq, FreqSet) of
        true ->
            {true, NewFreq};
        false ->
            find_first_duplicate(IntList, NewFreq, sets:add_element(NewFreq, FreqSet))
    end.
