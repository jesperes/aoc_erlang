%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(aoc2020_day11).

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
             , day = 11
             , name = "Seating System"
             , expected = {2093, 1862}
             , has_input_file = true
             }.

%%==============================================================================
%% Types
%%==============================================================================
-type coord() :: { X :: integer()
                 , Y :: integer()
                 }.
-type grid() :: #{coord() => integer()}.
-type input_type() :: grid().
-type result1_type() :: any().
-type result2_type() :: result1_type().

%%------------------------------------------------------------------------------
%% @doc parse/1
%% Parses input file.
%% @end
%%------------------------------------------------------------------------------
-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  to_map(string:tokens(binary_to_list(Input), "\n\r")).

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(Grid :: input_type()) -> result1_type().
solve1(Grid) ->
  iterate_until_same(Grid, fun compute_next1/3).

%%------------------------------------------------------------------------------
%% @doc solve2/1
%% Solves part 2. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve2(Grid :: input_type()) -> result2_type().
solve2(Grid) ->
  iterate_until_same(Grid, fun compute_next2/3).

%%==============================================================================
%% Internals
%%==============================================================================

%% Iterate until the grid does not change
iterate_until_same(Grid, Fun) ->
  Next = iterate_one_step(Grid, Fun),
  case Next =:= Grid of
    true ->
      maps:fold(fun(_, $#, Acc) -> Acc + 1;
                   (_, _, Acc) -> Acc
                end, 0, Next);
    false ->
      iterate_until_same(Next, Fun)
  end.

iterate_one_step(Grid, Fun) ->
  maps:fold(
    fun(K, V, Acc) ->
        maps:put(K, Fun(K, V, Grid), Acc)
    end, #{}, Grid).

%% Compute the next state of cell `V' at coordinate `Coord'.
compute_next1(Coord, V, OldGrid) ->
  OccupiedAdj = occupied_adjacents(Coord, OldGrid),
  case V of
    $L when OccupiedAdj == 0 -> $#;             % become occupied
    $# when OccupiedAdj >= 4 -> $L;             % become free
    _ -> V                                      % unchanged
  end.

occupied_adjacents({X, Y}, Grid) ->
  Deltas = [{-1, -1}, {0, -1}, {1, -1},
            {-1,  0},          {1,  0},
            {-1,  1}, {0, 1},  {1,  1}],

  lists:foldl(
    fun({Dx, Dy}, Acc) ->
        case maps:get({X + Dx, Y + Dy}, Grid, undefined) of
          $# -> Acc + 1;
          _ -> Acc
        end
    end, 0, Deltas).

%% Compute the next state of cell `V' at coordinate `Coord'.
compute_next2(Coord, V, OldGrid) ->
  VisibleAdj = length(visible_adjacents(Coord, OldGrid)),
  case V of
    $L when VisibleAdj == 0 -> $#;             % become occupied
    $# when VisibleAdj >= 5 -> $L;             % become free
    _ -> V                                     % unchanged
  end.

visible_adjacents(Coord, Grid) ->
  Deltas = [{-1, -1}, {0, -1}, {1, -1},
            {-1,  0},          {1,  0},
            {-1,  1}, {0, 1},  {1,  1}],

  lists:foldl(
    fun(Delta, Acc) ->
        case find_first_in_direction(Coord, Delta, 1, Grid) of
          {_, _} = Adj -> [Adj|Acc];
          false -> Acc
        end
    end, [], Deltas).

find_first_in_direction({X, Y} = Coord, {Dx, Dy} = Delta, Dist, Grid) ->
  VisibleCoord = {X + Dx * Dist, Y + Dy * Dist},
  case maps:get(VisibleCoord, Grid, undefined) of
    $# -> VisibleCoord;
    $. -> find_first_in_direction(Coord, Delta, Dist + 1, Grid);
    _ -> false
  end.

%% Parse input lines to a map
-spec to_map([string()]) -> grid().
to_map(Lines) ->
  {_, Grid} =
    lists:foldl(
      fun(L, {Y, Map}) ->
          {_, MapOut} =
            lists:foldl(
              fun(C, {X, Acc}) ->
                  {X + 1, maps:put({X, Y}, C, Acc)}
              end, {0, Map}, L),
          {Y + 1, MapOut}
      end, {0, #{}}, Lines),
  Grid.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
