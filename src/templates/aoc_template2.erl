-module(aoc_template2).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 0,
                day = 0,
                name = "TBD",
                expected = {0, 0},
                has_input_file = true}.

-type input_type() :: any().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    string:tokens(binary_to_list(Binary), "\n\r").

-spec solve1(Input :: input_type()) -> result_type().
solve1(_Input) ->
    0.

-spec solve2(Input :: input_type()) -> result_type().
solve2(_Input) ->
    0.

%% Tests
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

%% ...

-endif.