-module(aoc2015_day02).

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 2,
                name = "I Was Told There Would Be No Math",
                expected = {1586300, 3737498},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: binary().
-type result_type() :: {integer(), integer()}.

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    Input.

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    LineFun = fun(L) -> lists:map(fun list_to_integer/1, string:tokens(L, "x")) end,
    lists:foldl(fun(Line, {A1, A2}) ->
                   [X, Y, Z] = LineFun(Line),
                   {A1 + 2 * X * Y + 2 * Y * Z + 2 * Z * X + lists:min([X * Y, Y * Z, Z * X]),
                    A2 + 2 * lists:min([X + Y, Y + Z, Z + X]) + X * Y * Z}
                end,
                {0, 0},
                string:tokens(binary_to_list(Input), "\n\r")).
