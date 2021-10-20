-module(aoc2017_day11).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 11,
                name = "Hex Ed",
                expected = {685, 1457},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: [atom()].
-type result_type() :: {integer(), integer()}.

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    string:tokens(
        string:trim(binary_to_list(Binary)), ",").

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    {FinalPos, MaxDist} =
        lists:foldl(fun(Move, {{X, Y, Z}, MaxDist}) ->
                       NewPos =
                           case Move of
                               "n" -> {X + 1, Y, Z - 1};
                               "ne" -> {X + 1, Y - 1, Z};
                               "se" -> {X, Y - 1, Z + 1};
                               "s" -> {X - 1, Y, Z + 1};
                               "sw" -> {X - 1, Y + 1, Z};
                               "nw" -> {X, Y + 1, Z - 1}
                           end,
                       {NewPos, max(MaxDist, manhattan_dist(NewPos))}
                    end,
                    {{0, 0, 0}, 0},
                    Input),
    {manhattan_dist(FinalPos), MaxDist}.

manhattan_dist({X, Y, Z}) ->
    floor((abs(X) + abs(Y) + abs(Z)) / 2).
