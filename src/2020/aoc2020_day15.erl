%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(aoc2020_day15).

-behavior(aoc_puzzle).

-export([ parse/1
        , solve1/1
        , solve2/1
        , info/0
        ]).

-include("aoc_puzzle.hrl").

%%------------------------------------------------------------------------------
%% @doc info/0
%% Returns info about this puzzle.
%% @end
%%------------------------------------------------------------------------------
-spec info() -> aoc_puzzle().
info() ->
  #aoc_puzzle{ module = ?MODULE
             , year = 2020
             , day = 15
             , name = "Rambunctious Recitation"
             , expected = {475, 11261}
             , has_input_file = false
             }.

%%==============================================================================
%% Types
%%==============================================================================
-type input_type() :: [integer()].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

%%------------------------------------------------------------------------------
%% @doc parse/1
%% Parses input file.
%% @end
%%------------------------------------------------------------------------------
-spec parse(binary()) -> input_type().
parse(_Input) ->
  [6,4,12,1,20,0,16].

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
  solve(Input, 2020).

%%------------------------------------------------------------------------------
%% @doc solve2/1
%% Solves part 2. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
  solve(Input, 30000000).

%%==============================================================================
%% Internals
%%==============================================================================

solve(Input, Limit) ->
  [Last|Rest] = lists:reverse(Input),
  Input0 = lists:reverse(Rest),

  Array = array_new(Limit),
  lists:foreach(
    fun({Turn, Num}) ->
        array_put(Array, Num, Turn)
    end, lists:zip(lists:seq(1, length(Input0)),
                   Input0)),

  NextTurn = length(Input) + 1,
  solve0(NextTurn, Last, Array, Limit).

solve0(Turn, Last, _, Limit) when Turn > Limit ->
  Last;
solve0(Turn, Last, Array, Limit) ->
  Next =
    case array_get(Array, Last) of
      0 -> 0;
      IndexOfLast -> Turn - IndexOfLast - 1
    end,
  array_put(Array, Last, Turn - 1),
  solve0(Turn + 1, Next, Array, Limit).

array_new(Size) ->
  atomics:new(Size + 1, []).

array_put(Array, I, Val) ->
  ok = atomics:put(Array, I + 1, Val).

array_get(Array, I) ->
  atomics:get(Array, I + 1).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
