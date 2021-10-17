%%% Advent of Code solution for 2020 day 01.
%%% Created: 2020-12-01T07:09:55+00:00

-module(aoc2020_day01).
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
             , day = 1
             , name = "Report Repair"
             , expected = {987339, 259521570}
             , has_input_file = true
             }.

-type input_type() :: [integer()].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  lists:map(fun list_to_integer/1,
            string:tokens(binary_to_list(Input), "\n\r")).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
  hd([X * Y || X <- Input,
               Y <- Input,
               X < Y,
               X + Y == 2020]).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
  Min = lists:min(Input),
  hd([X * Y * Z || X <- Input,
                   Y <- Input,
                   X < Y,
                   X + Y < (2020 - Min),
                   Z <- Input,
                   Y < Z,
                   X + Y + Z == 2020]).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
