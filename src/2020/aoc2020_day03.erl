%%% Advent of Code solution for 2020 day 03.
%%% Created: 2020-12-03T07:01:04+00:00

-module(aoc2020_day03).
-behavior(aoc_puzzle).

-export([ parse/1
        , solve1/1
        , solve2/1
        , info/0
        ]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
  #aoc_puzzle{ module = ?MODULE
             , year = 2020
             , day = 3
             , name = "Toboggan Trajectory"
             , expected = {230, 9533698720}
             , has_input_file = true
             }.

-type input_type() :: [string()].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  string:tokens(binary_to_list(Input), "\r\n").

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
  num_trees(Input, {3, 1}).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
  num_trees(Input, {1, 1}) *
    num_trees(Input, {3, 1}) *
    num_trees(Input, {5, 1}) *
    num_trees(Input, {7, 1}) *
    num_trees(Input, {1, 2}).

num_trees(Lines, Slope) ->
  num_trees(Lines, 0, 0, 0, Slope).

num_trees(Lines, _X, Y, NumTrees, _Slope)
  when Y >= length(Lines) ->
  NumTrees;
num_trees(Lines, X, Y, NumTrees, {SlopeX, SlopeY} = Slope) ->
  Line = lists:nth(Y + 1, Lines),
  Width = length(Line),
  NumTrees1 =
    case lists:nth((X rem Width) + 1, Line) of
      $# -> NumTrees + 1;
      _ -> NumTrees
    end,
  num_trees(Lines, X + SlopeX, Y + SlopeY, NumTrees1, Slope).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
