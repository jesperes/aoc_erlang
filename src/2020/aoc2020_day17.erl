%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(aoc2020_day17).

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
             , day = 17
             , name = "Conway Cubes"
             , expected = {242, 2292}
             , has_input_file = false
             }.

%%==============================================================================
%% Types
%%==============================================================================
-type input_type() :: binary().
-type result1_type() :: integer().
-type result2_type() :: result1_type().

%%------------------------------------------------------------------------------
%% @doc parse/1
%% Parses input file.
%% @end
%%------------------------------------------------------------------------------
-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
  <<"..#..#..\n"
    ".###..#.\n"
    "#..##.#.\n"
    "#.#.#.#.\n"
    ".#..###.\n"
    ".....#..\n"
    "#...####\n"
    "##....#.\n">>.

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
  Cubes = parse_3d(Input),
  conway_3d(Cubes, 6).

%%------------------------------------------------------------------------------
%% @doc solve2/1
%% Solves part 2. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
  Cubes = parse_4d(Input),
  conway_4d(Cubes, 6).

%% ======================================================================
%% 3D conways game of life
%% ======================================================================

conway_3d(Cubes, 0) ->
  maps:size(Cubes);
conway_3d(Cubes, N) ->
  Cubes0 = do_conway_3d_step(Cubes),
  %% show_layers(Cubes0),
  conway_3d(Cubes0, N - 1).

do_conway_3d_step(Cubes) ->
  Bound = get_bound(Cubes),

  Coords =
    [{X, Y, Z} ||
      X <- lists:seq(-Bound, Bound),
      Y <- lists:seq(-Bound, Bound),
      Z <- lists:seq(-Bound, Bound)],
  lists:foldl(fun(Coord, Acc) ->
                  step_one_cube(Coord, Cubes, Acc)
              end, #{}, Coords).

step_one_cube(Coord, OldCubes, NewCubes) ->
  Nbrs = get_neighbors(Coord),
  NumActiveNbrs =
    lists:foldl(
      fun(Nbr, Acc) ->
          case maps:is_key(Nbr, OldCubes) of
            true -> Acc + 1;
            _ -> Acc
          end
      end, 0, Nbrs),

  WasActive = maps:is_key(Coord, OldCubes),
  case {WasActive, NumActiveNbrs} of
    {true, N} when (N == 2) or (N == 3) ->
      maps:put(Coord, $#, NewCubes);
    {false, N} when (N == 3) ->
      maps:put(Coord, $#, NewCubes);
    _ ->
      NewCubes
  end.

get_neighbors({X, Y, Z}) ->
  [{XN, YN, ZN} ||
    XN <- [X - 1, X, X + 1],
    YN <- [Y - 1, Y, Y + 1],
    ZN <- [Z - 1, Z, Z + 1],
    not ((XN == X) and (YN == Y) and (ZN == Z))].

get_bound(Cubes) ->
  1 + maps:fold(
        fun({X, Y, Z}, _, Acc) ->
            lists:max([Acc, abs(X), abs(Y), abs(Z)])
        end, 0, Cubes).

%% ======================================================================
%% 4D conways game of life (brain explodes)
%% ======================================================================

conway_4d(Cubes, 0) ->
  maps:size(Cubes);
conway_4d(Cubes, N) ->
  Cubes0 = do_conway_4d_step(Cubes),
  %% show_layers(Cubes0),
  conway_4d(Cubes0, N - 1).

do_conway_4d_step(Cubes) ->
  {{MinX, MaxX},
   {MinY, MaxY},
   {MinZ, MaxZ},
   {MinW, MaxW}} = get_bound_4d(Cubes),

  Coords =
    [{X, Y, Z, W} ||
      X <- lists:seq(MinX, MaxX),
      Y <- lists:seq(MinY, MaxY),
      Z <- lists:seq(MinZ, MaxZ),
      W <- lists:seq(MinW, MaxW)],

  lists:foldl(fun(Coord, Acc) ->
                  step_one_cube_4d(Coord, Cubes, Acc)
              end, #{}, Coords).

get_bound_4d(Cubes) ->
  {{MinX, MaxX},
   {MinY, MaxY},
   {MinZ, MaxZ},
   {MinW, MaxW}} =
    maps:fold(
      fun({X, Y, Z, W}, _, {{MinX, MaxX},
                            {MinY, MaxY},
                            {MinZ, MaxZ},
                            {MinW, MaxW}}) ->
          {{min(X, MinX), max(X, MaxX)},
           {min(Y, MinY), max(Y, MaxY)},
           {min(Z, MinZ), max(Z, MaxZ)},
           {min(W, MinW), max(W, MaxW)}}
      end, {{0, 0}, {0, 0}, {0, 0}, {0, 0}}, Cubes),
  {{MinX - 1, MaxX + 1},
   {MinY - 1, MaxY + 1},
   {MinZ - 1, MaxZ + 1},
   {MinW - 1, MaxW + 1}}.

step_one_cube_4d(Coord, OldCubes, NewCubes) ->
  Nbrs = get_neighbors_4d(Coord),
  NumActiveNbrs =
    lists:foldl(
      fun(Nbr, Acc) ->
          case maps:is_key(Nbr, OldCubes) of
            true -> Acc + 1;
            _ -> Acc
          end
      end, 0, Nbrs),

  WasActive = maps:is_key(Coord, OldCubes),
  case {WasActive, NumActiveNbrs} of
    {true, N} when (N == 2) or (N == 3) ->
      maps:put(Coord, $#, NewCubes);
    {false, N} when (N == 3) ->
      maps:put(Coord, $#, NewCubes);
    _ ->
      NewCubes
  end.

get_neighbors_4d({X, Y, Z, W}) ->
  [{XN, YN, ZN, WN} ||
    XN <- [X - 1, X, X + 1],
    YN <- [Y - 1, Y, Y + 1],
    ZN <- [Z - 1, Z, Z + 1],
    WN <- [W - 1, W, W + 1],
    not ((XN == X) and (YN == Y) and (ZN == Z) and (WN == W))].

%% ======================================================================
%% Parser
%% ======================================================================

parse_3d(Binary) ->
  [L|_] = binary:split(Binary, <<"\n">>, [global]),
  W = byte_size(L) + 1,
  lists:foldl(fun({Pos, _}, Map) ->
                  X = Pos rem W,
                  Y = Pos div W,
                  Z = 0,
                  maps:put({X, Y, Z}, $#, Map)
              end, #{}, binary:matches(Binary, <<"#">>)).

parse_4d(Binary) ->
  [L|_] = binary:split(Binary, <<"\n">>, [global]),
  W = byte_size(L) + 1,
  lists:foldl(fun({Pos, _}, Map) ->
                  X = Pos rem W,
                  Y = Pos div W,
                  Z = 0,
                  Z = 0,
                  maps:put({X, Y, Z, W}, $#, Map)
              end, #{}, binary:matches(Binary, <<"#">>)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
