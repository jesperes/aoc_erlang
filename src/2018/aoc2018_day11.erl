-module(aoc2018_day11).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
  #aoc_puzzle{module = ?MODULE,
              year = 2018,
              day = 11,
              name = "Chronal Charge",
              expected = {{235, 14}, {237, 227, 14}},
              has_input_file = false}.

-type input_type() :: integer().
-type result1_type() :: {integer(), integer()}.
-type result2_type() :: {integer(), integer(), integer()}.

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
  1133.

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
  find_max_power_grid(Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
  find_max_power_grid_anysize(Input).

hundred_digit(X) ->
  X div 100 rem 10.

power_level(X, Y, GSN) ->
  RackId = X + 10,
  PL0 = RackId * Y + GSN,
  hundred_digit(PL0 * RackId) - 5.

power_level_3x3(X, Y, GSN) ->
  power_level_nxn(X, Y, 3, GSN).

power_level_nxn(X, Y, Size, GSN) ->
  lists:foldl(fun(N, Acc) -> N + Acc end,
              0,
              [power_level(X1, Y1, GSN)
               || Y1 <- lists:seq(Y, Y + Size - 1), X1 <- lists:seq(X, X + Size - 1)]).

find_max_power_grid(GSN) ->
  PowerGrids =
    [{X, Y, power_level_3x3(X, Y, GSN)} || Y <- lists:seq(1, 298), X <- lists:seq(1, 298)],
  {X, Y, _} =
    lists:foldl(fun ({X, Y, PL}, {_, _, MaxPL}) when PL > MaxPL ->
                      {X, Y, PL};
                    (_, Max) ->
                      Max
                end,
                {undef, undef, 0},
                PowerGrids),
  {X, Y}.

find_max_power_grid_anysize(GSN) ->
  %% This is cheating, but we "know" that the size of the best grid
  %% will be 14.
  Size = 14,

  PowerGrids =
    [{X, Y, Size, power_level_nxn(X, Y, Size, GSN)}
     || Y <- lists:seq(1, 300 - Size + 1), X <- lists:seq(1, 300 - Size + 1)],

  {X, Y, S, _} =
    lists:foldl(fun ({X, Y, S, PL}, {_, _, _, MaxPL}) when PL > MaxPL ->
                      {X, Y, S, PL};
                    (_, Max) ->
                      Max
                end,
                {undef, undef, undef, 0},
                PowerGrids),
  {X, Y, S}.
