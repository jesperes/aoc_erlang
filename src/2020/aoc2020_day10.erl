%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(aoc2020_day10).
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
             , day = 10
             , name = "Adapter Array"
             , expected = {2738, 74049191673856}
             , has_input_file = true
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
-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  lists:map(fun erlang:list_to_integer/1,
            string:tokens(binary_to_list(Input), "\n\r")).

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(List :: input_type()) -> result1_type().
solve1(List) ->
  Adapters = lists:sort(List),
  #{3 := D3, 1 := D1} = adapt(0, Adapters, #{}),
  D3 * D1.

%%------------------------------------------------------------------------------
%% @doc solve2/1
%% Solves part 2. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve2(List :: input_type()) -> result2_type().
solve2(List) ->
  Adapters = lists:sort(List),
  Device = lists:max(Adapters) + 3,

  %% Idea borrowed from
  %% https://www.reddit.com/r/adventofcode/comments/ka8z8x/2020_day_10_solutions/gf990qj/

  %% Cache is a maps from indexes in `Adapters' to the number
  %% of ways it can be reached.
  Cache = #{0 => 1},

  %% For each adapter, add the number of ways the previous three
  %% adapters can be reached.
  Cache0 =
    lists:foldl(fun(I, CacheIn) ->
                    Sum3 = lists:sum([maps:get(I - X, CacheIn, 0) ||
                                       X <- [1, 2, 3], I - X >= 0]),
                    maps:put(I, Sum3, CacheIn)
                end, Cache, Adapters ++ [Device]),

  lists:max(maps:values(Cache0)).

%%==============================================================================
%% Internals
%%==============================================================================

adapt(_Jolts, [], Acc) ->
  incr(3, Acc); %% include last 3-diff to device
adapt(Jolts, [Next|List], Acc) ->
  Diff = Next - Jolts,
  adapt(Jolts + Diff, List, incr(Diff, Acc)).

incr1(N) -> N + 1.

incr(N, Map) ->
  maps:update_with(N, fun incr1/1, 1, Map).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
