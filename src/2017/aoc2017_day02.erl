-module(aoc2017_day02).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 2,
                name = "Corruption Checksum",
                expected = {37923, 263},
                has_input_file = true}.

-type input_type() :: [[integer()]].
-type result1_type() :: any().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    lists:map(fun(Line) -> lists:map(fun list_to_integer/1, string:tokens(Line, "\t")) end,
              string:tokens(binary_to_list(Input), "\n\r")).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    lists:foldl(fun(Line, Acc) -> Acc + abs(lists:min(Line) - lists:max(Line)) end, 0, Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    lists:foldl(fun(Line, Acc) ->
                   Acc + lists:sum([A div B || A <- Line, B <- Line, A > B, A rem B == 0])
                end,
                0,
                Input).
