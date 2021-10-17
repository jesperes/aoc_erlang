-module(aoc2015_day25).

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 25,
                name = "Let It Snow",
                expected = 8997277,
                use_one_solver_fun = true,
                has_input_file = false}.

-type input_type() :: {X :: integer(), Y :: integer()}.
-type result_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    {3019, 3010}.

-spec solve(Input :: input_type()) -> result_type().
solve({X, Y}) ->
    find_value(X, Y).

%%% Implementation

init_value() -> 20151125.
next_value(Prev) -> (Prev * 252533) rem 33554393.

find_value(1, 1) -> init_value();
find_value(1, Y) -> next_value(find_value(Y - 1, 1));
find_value(X, Y) -> next_value(find_value(X - 1, Y + 1)).
