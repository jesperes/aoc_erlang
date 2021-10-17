%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(aoc2020_day20).

-behavior(aoc_puzzle).

-export([ parse/1
        , solve1/1
        , solve2/1
        , info/0
        ]).

-include_lib("stdlib/include/assert.hrl").
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
             , day = 20
             , name = "Jurassic Jigsaw"
             , expected = {66020135789767, 1537}
             , has_input_file = true
             }.

%%==============================================================================
%% Types
%%==============================================================================
-type tile_id() :: {TileNum :: integer(),
                    Symmetry :: atom()}.
-type input_type() :: #{tile_id() => map()}.
-type result1_type() :: integer().
-type result2_type() :: result1_type().

%%------------------------------------------------------------------------------
%% @doc parse/1
%% Parses input file.
%% @end
%%------------------------------------------------------------------------------
-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  L = binary:split(Input, <<"Tile ">>, [global]),
  lists:foldl(fun(<<>>, Acc) -> Acc;
                 (TileBin, Acc) ->
                  maps:merge(Acc, parse_tile(TileBin))
              end, #{}, L).

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(Tiles :: input_type()) -> result1_type().
solve1(Tiles) ->
  Size = floor(math:sqrt(maps:size(Tiles) div 8)),
  PlacedTiles = place_tiles(Tiles),
  lists:foldl(
    fun(Coord, Acc) ->
        {TileId, _} = maps:get(Coord, PlacedTiles),
        Acc * TileId
    end, 1, [{0, 0},
             {Size - 1, 0},
             {0, Size - 1},
             {Size - 1, Size - 1}]).

%%------------------------------------------------------------------------------
%% @doc solve2/1
%% Solves part 2. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve2(Tiles :: input_type()) -> result2_type().
solve2(Tiles) ->
  Size = floor(math:sqrt(maps:size(Tiles) div 8)),
  PlacedTiles = place_tiles(Tiles),
  FinalGrid = join_tiles(PlacedTiles, Tiles, Size),
  SM = sea_monster(),
  maps:fold(
    fun(_Id, Grid, not_found) ->
        AllHashes = sets:from_list(maps:keys(Grid) -- [size]),
        NumHashes = sets:size(AllHashes),
        RemainingPixels = find_sea_monster(SM, Grid, AllHashes),
        case sets:size(RemainingPixels) of
          N when N < NumHashes -> N;
          _ -> not_found
        end;
       (_Id, _Grid, Solution) -> Solution
    end, not_found, all_symmetries(final, FinalGrid)).

%%==============================================================================
%% Helpers
%%==============================================================================

place_tiles(Tiles) ->
  Size = floor(math:sqrt(maps:size(Tiles) div 8)),
  [First|_Rest] = [{X, Y} || X <- lists:seq(0, Size - 1),
                            Y <- lists:seq(0, Size - 1)],
  BorderMap = border_map(Tiles),
  InvBorderMap = inv_border_map(Tiles),
  [StartTile|_] = find_ne_corner_tile(Tiles, BorderMap, InvBorderMap),

  %% Place the first tile
  PlacedTiles = #{First => StartTile},
  RemainingTiles = remove_tile(StartTile, Tiles),

  %% Place remaining tiles, row-by-row
  place_row(0, Size, StartTile, RemainingTiles,
            BorderMap, PlacedTiles).

%% Build coord-map of the sea monster.
sea_monster() ->
  Width = 20,
  Lines = <<"                  # ",
            "#    ##    ##    ###",
            " #  #  #  #  #  #   ">>,

  %% Coordinates in the resulting map are relative to the tip of the
  %% sea monster's tail
  lists:foldl(
    fun({Offset, _}, Acc) ->
        maps:put({Offset rem Width,
                  (Offset div Width) - 1}, $#, Acc)
    end, #{}, binary:matches(Lines, <<"#">>)).

%% Find sea monsters and remove any matching `#' from `AllHashes'.
find_sea_monster(SM, FinalGrid, AllHashes) ->
  maps:fold(
    fun(size, _, Acc) -> Acc;
       (Coord, _, Acc) ->
        is_sea_monster_at(Coord, SM, FinalGrid, Acc)
    end, AllHashes, FinalGrid).

%% If there is a sea monster at {X, Y}, remove all hashes from `AllHashes'
%% which matches the sea monster pixels.
is_sea_monster_at({X, Y}, SM, Grid, AllHashes) ->
  NumSMPixels = maps:size(SM),
  MatchingPixels =
    lists:foldl(fun({SMX, SMY}, Acc) ->
                    Coord = {X + SMX, Y + SMY},
                    case maps:is_key(Coord, Grid) of
                      true -> [Coord|Acc];
                      false -> Acc
                    end
                end, [], maps:keys(SM)),
  case length(MatchingPixels) of
    N when N == NumSMPixels ->
      %% All pixels matched, remove them from `AllHashes'
      sets:subtract(AllHashes, sets:from_list(MatchingPixels));
    _ ->
      AllHashes
  end.

%% Join all the placed tiles into a big jigsaw.
join_tiles(PlacedTiles, Tiles, Size) ->
  lists:foldl(
    fun({X, Y} = Coord, Acc) ->
        %% {X, Y} are here the coordinates of the tiles themselves within
        %% the puzzle

        TileId = maps:get(Coord, PlacedTiles),
        TileData = maps:get(TileId, Tiles),
        Width = maps:get(size, TileData),
        InnerWidth = Width - 2,
        MaxN = Width - 1,

        Acc0 = maps:put(size, InnerWidth * Size, Acc),

        maps:fold(
          fun({X0, Y0}, _, InnerAcc) ->
              %% {X0, Y0} are the coordinates of each pixel within the
              %% tile

              if (X0 == 0) orelse (X0 == MaxN) orelse
                 (Y0 == 0) orelse (Y0 == MaxN) ->
                  InnerAcc;
                 true ->
                  maps:put({(X * InnerWidth) + X0 - 1,
                            (Y * InnerWidth) + Y0 - 1},
                           $#, InnerAcc)
              end;
             (_, _, InnerAcc) ->
              InnerAcc
          end, Acc0, TileData)
    end, #{},
    [{X, Y} || X <- lists:seq(0, Size - 1),
               Y <- lists:seq(0, Size - 1)]).



remove_tile({Num, _}, Tiles) ->
  maps:filter(fun({TileNum, _}, _) when Num =/= TileNum -> true;
                 (_Id, _) -> false
              end, Tiles).

%% place_row: This is the tricky part. Given `LeftTile`, finds next
%% tile to the right, places it, and continues until the row runs
%% out. Then, picks the next tile in the row underneath, and does the
%% same thing.
place_row(Y, Size, LeftTile, RemainingTiles, BorderMap, PlacedTiles) ->
  {_,
   PlacedTilesOut0,
   RemainingTilesOut0} =
    lists:foldl(
      fun(X, {LeftTileIn, PlacedTilesIn, RemainingTilesIn}) ->
          [_N, _S, E, _W] = maps:get(LeftTileIn, BorderMap),
          Coord = {X, Y},
          {LeftNum, _} = LeftTileIn,
          [{RightId, _RightData}] =
            maps:to_list(
              maps:filter(
                fun({Num, _} = TileId, _TD) when Num =/= LeftNum ->
                    case maps:get(TileId, BorderMap) of
                      [_, _, _, RightW] when RightW == E -> true;
                      _ -> false
                    end;
                   (_, _) -> false
                end, RemainingTilesIn)),

          PlacedTilesOut = maps:put(Coord, RightId, PlacedTilesIn),
          RemainingTilesOut = remove_tile(RightId, RemainingTilesIn),
          LeftTileOut = RightId,

          {LeftTileOut, PlacedTilesOut, RemainingTilesOut}
      end, {LeftTile, PlacedTiles, RemainingTiles}, lists:seq(1, Size - 1)),

  if Y + 1 == Size ->
      %% No more rows to place
      PlacedTilesOut0;
     true ->
      NewLeftTile = find_tile_below(LeftTile, RemainingTilesOut0, BorderMap),
      Coord0 = {0, Y + 1},

      PlacedTilesOut1 = maps:put(Coord0, NewLeftTile, PlacedTilesOut0),
      RemainingTilesOut1 = remove_tile(NewLeftTile, RemainingTilesOut0),

      place_row(Y + 1, Size, NewLeftTile, RemainingTilesOut1, BorderMap, PlacedTilesOut1)
  end.

find_tile_below(Tile, RemainingTiles, BorderMap) ->
  [_, S, _, _] = maps:get(Tile, BorderMap),
  [{BelowId, _}] =
    maps:to_list(
      maps:filter(
        fun({_Num, _} = TileId, _TD) ->
            [BelowN|_] = maps:get(TileId, BorderMap),
            BelowN == S
        end, RemainingTiles)),
  BelowId.


find_ne_corner_tile(Tiles, BorderMap, InvBorderMap) ->
  maps:fold(
    fun(TileId, _TileData, Acc) ->
        Borders = maps:get(TileId, BorderMap),
        case lists:map(
               fun(BorderId) ->
                   case is_external_border(BorderId, InvBorderMap) of
                     true -> external;
                     false -> BorderId
                   end
               end, Borders) of
          %% Start with NW (top left) tile
          %% Order is N S E W
          [external, _S, _E, external] -> [TileId|Acc];
          _Other -> Acc
        end
    end, [], Tiles).


%% External borders are borders which only belong to one tile.
is_external_border(BorderId, InvBorderMap) ->
  length(maps:get(BorderId, InvBorderMap)) == 1.

%% Inverse border map; maps border ids to their tile numbers
inv_border_map(Tiles) ->
  maps:fold(
    fun({TileNum, _Sym}, Data, Acc) ->
        Borders = borders(Data),
        lists:foldl(
          fun(Border, InnerAcc) ->
              maps:update_with(
                Border,
                fun(Old) -> lists:usort([TileNum|Old]) end,
                [TileNum], InnerAcc)
          end, Acc, Borders)
    end, #{}, Tiles).

%% Return a map of tile ids to their possible border ids
border_map(Tiles) ->
  maps:fold(fun(TileId, TileData, Acc) ->
                maps:put(TileId, borders(TileData), Acc)
            end, #{}, Tiles).

%% ======================================================================
%% Parser
%% ======================================================================

parse_tile(TileBin) ->
  [Header, Rows] = binary:split(TileBin, <<"\n">>),
  {match, Matches} =
    re:run(Header, "(\\d+):", [{capture, all_but_first, list}]),
  TileNum = list_to_integer(hd(Matches)),
  [{Width, _}|_] = binary:matches(Rows, <<"\n">>),
  Offsets = binary:matches(Rows, <<"#">>),
  TileData =
    lists:foldl(fun({Offset, _}, Acc) ->
                    maps:put({Offset rem (Width + 1),
                              Offset div (Width + 1)}, $#, Acc)
                end, #{}, Offsets),

  all_symmetries(TileNum, maps:put(size, Width, TileData)).

shift_by_coord(Coord, Tile, N) ->
  case maps:get(Coord, Tile, undefined) of
    $# -> (N bsl 1) bor 1;
    _ -> N bsl 1
  end.

borders(Tile) ->
  Size = maps:get(size, Tile),
  L = lists:seq(0, Size - 1),
  N = lists:foldl(fun(X, Acc) -> shift_by_coord({X, 0},        Tile, Acc) end, 0, L),
  S = lists:foldl(fun(X, Acc) -> shift_by_coord({X, Size - 1}, Tile, Acc) end, 0, L),
  E = lists:foldl(fun(Y, Acc) -> shift_by_coord({Size - 1, Y}, Tile, Acc) end, 0, L),
  W = lists:foldl(fun(Y, Acc) -> shift_by_coord({0, Y},        Tile, Acc) end, 0, L),
  [N, S, E, W].

%% ======================================================================
%% Helpers
%% ======================================================================

all_symmetries(Num, Rows) ->
  TileSize = maps:get(size, Rows),
  Max = TileSize - 1,

  Rotate =
    fun(R) ->
        maps:fold(fun({X, Y}, Value, Acc) when (X >= 0) andalso (X =< Max) andalso
                                               (Y >= 0) andalso (Y =< Max) ->
                      maps:put({Max - Y, X}, Value, Acc);
                     (K, V, Acc) ->
                      maps:put(K, V, Acc)
                  end, #{}, R)
    end,

  Flip =
    fun(R) ->
        maps:fold(fun({X, Y}, Value, Acc) when (X >= 0) andalso (X =< Max) andalso
                                               (Y >= 0) andalso (Y =< Max) ->
                      maps:put({Max - X, Y}, Value, Acc);
                     (K, V, Acc) ->
                      maps:put(K, V, Acc)
                  end, #{}, R)
    end,

  R90      = Rotate(Rows),
  R180     = Rotate(R90),
  R270     = Rotate(R180),
  FlipR0   = Flip(Rows),
  FlipR90  = Rotate(FlipR0),
  FlipR180 = Rotate(FlipR90),
  FlipR270 = Rotate(FlipR180),

  %% Self-test
  ?assertEqual(Rows, Rotate(R270)),
  ?assertEqual(Rows, Rotate(Rotate(Rotate(Rotate(Rows))))),

  #{{Num, 'r0'}   => Rows,
    {Num, 'r90'}  => R90,
    {Num, 'r180'} => R180,
    {Num, 'r270'} => R270,
    {Num, 'f0'}   => FlipR0,
    {Num, 'f90'}  => FlipR90,
    {Num, 'f180'} => FlipR180,
    {Num, 'f270'} => FlipR270}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
