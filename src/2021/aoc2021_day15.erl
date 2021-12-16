-module(aoc2021_day15).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 15,
                name = "Chiton",
                expected = {386, 2806},
                has_input_file = true}.

-type input_type() :: any().
-type result_type() :: integer().

-define(WIDTH, 100).
-define(HEIGHT, 100).
-define(IS_GOAL(X, Y, Tiles), X == ?WIDTH * Tiles - 1 andalso Y == ?HEIGHT * Tiles - 1).
-define(DELTAS, [{-1, 0}, {0, -1}, {1, 0}, {0, 1}]).
% Code the entries in the open set into a single 32-bit integer instead of a
% {Dist, {X, Y}} tuple. This seems to gain a bit in performance.
-define(PACK_F(Dist, X, Y), Dist bsl 24 bor (X bsl 12) bor Y).
-define(UNPACK_F(F), {F bsr 24, {(F bsr 12) band 16#fff, F band 16#fff}}).
% Pack coordinates into an 32-bit integer too, using them as keys in the G set.
-define(PACK_COORD(X, Y), X bsl 12 bor Y).

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    Binary.

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    find(Input, 1).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    find(Input, 5).

find(Input, Tiles) ->
    ?assertEqual($\n, binary:at(Input, ?WIDTH)),
    Start = {0, 0},
    find(#{?PACK_COORD(0, 0) => 0}, % actual cost to position
         gb_sets:singleton(?PACK_F(lower_bound_dist_to_goal(Start, Tiles), 0, 0)),
         Input,
         Tiles).

%% Cost of moving to a coordinate.
edge_weight({X, Y}, Grid) ->
    X0 = X rem ?WIDTH,
    Y0 = Y rem ?HEIGHT,
    TileX = X div ?WIDTH,
    TileY = Y div ?HEIGHT,
    Risk = binary:at(Grid, Y0 * (?WIDTH + 1) + X0) - $0,
    (Risk + TileX + TileY - 1) rem 9 + 1.

lower_bound_dist_to_goal({X, Y}, Tiles) ->
    GoalX = ?WIDTH * Tiles - 1,
    GoalY = ?HEIGHT * Tiles - 1,
    abs(GoalX - X) + abs(GoalY - Y).

%% A* implementation
find(Gs, Fs, Grid, Tiles) ->
    Width = ?WIDTH * Tiles,
    Height = ?HEIGHT * Tiles,
    {F, Fs0} = gb_sets:take_smallest(Fs),
    {Dist, Curr} = ?UNPACK_F(F),
    case Curr of
        {X, Y} when ?IS_GOAL(X, Y, Tiles) ->
            Dist;
        {X, Y} ->
            PackedXY = ?PACK_COORD(X, Y),
            {NewGs, NewFs} =
                lists:foldl(fun ({Xa, Ya} = Coord, {GsIn, FsIn} = Acc)
                                    when Xa >= 0
                                         andalso Xa < Width
                                         andalso Ya >= 0
                                         andalso Ya < Height ->
                                    PackedCoord = ?PACK_COORD(Xa, Ya),
                                    MaybeNewScore =
                                        maps:get(PackedXY, GsIn) + edge_weight(Coord, Grid),
                                    case MaybeNewScore < maps:get(PackedCoord, GsIn, infinity) of
                                        true ->
                                            %% This path is better than previously known
                                            BetterScore = MaybeNewScore,
                                            GsOut = maps:put(PackedCoord, BetterScore, GsIn),
                                            NewDist =
                                                BetterScore
                                                + lower_bound_dist_to_goal(Coord, Tiles),
                                            FsOut = gb_sets:add(?PACK_F(NewDist, Xa, Ya), FsIn),
                                            {GsOut, FsOut};
                                        false ->
                                            Acc
                                    end;
                                (_, Acc) ->
                                    %% Neighbor out of range
                                    Acc
                            end,
                            {Gs, Fs0},
                            lists:map(fun({Dx, Dy}) -> {X + Dx, Y + Dy} end, ?DELTAS)),
            find(NewGs, NewFs, Grid, Tiles)
    end.

-ifdef(TEST).

f_key_test() ->
    [begin
         F = ?PACK_F(Dist, X, Y),
         Out = ?UNPACK_F(F),
         ?assertEqual(Out, {Dist, {X, Y}})
     end
     || Dist <- lists:seq(1000, 1100), X <- lists:seq(500, 510), Y <- lists:seq(500, 600, 10)].

-endif.
