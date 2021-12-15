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
                expected = {386, 0},
                has_input_file = true}.

-type input_type() :: any().
-type result_type() :: integer().

-define(WIDTH, 100).
-define(HEIGHT, 100).
-define(IS_GOAL(X, Y, Tiles), X == ?WIDTH * Tiles - 1 andalso Y == ?HEIGHT * Tiles - 1).
-define(DELTAS, [{-1, 0}, {0, -1}, {1, 0}, {0, 1}]).

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    Binary.

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    find(Input, 1).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    find(Input, 5).

%% 1741 too low

find(Input, Tiles) ->
    ?assertEqual($\n, binary:at(Input, ?WIDTH)),
    Start = {0, 0},
    find(#{Start => 0}, % actual cost to position
         gb_sets:singleton({lower_bound_dist_to_goal(Start, Tiles), Start}),
         Input,
         Tiles).

%% Cost of moving to a coordinate.
edge_weight({X, Y}, Grid) ->
    X0 = X rem ?WIDTH,
    Y0 = Y rem ?HEIGHT,
    TileX = X div ?WIDTH,
    TileY = Y div ?HEIGHT,
    Offset = Y0 * (?WIDTH + 1) + X0,
    Byte = binary:at(Grid, Offset),
    Risk = Byte - $0,
    % erlang:display({{byte, Byte}, {offset, Offset}, {x, X}, {y, Y}, {tile_x, TileX}, {tile_y, TileY}, {risk, Risk}}),
    ?assert(Byte =/= $\n),
    ?assert(Risk >= 1),
    case Risk + TileX + TileY of
        R when R > 9 ->
            1;
        R ->
            R
    end.

lower_bound_dist_to_goal({X, Y}, Tiles) ->
    GoalX = ?WIDTH * Tiles - 1,
    GoalY = ?HEIGHT * Tiles - 1,
    abs(GoalX - X) + abs(GoalY - Y).

%% A* implementation
find(Gs, Fs, Grid, Tiles) ->
    {{Dist, Curr} = _P, Fs0} = gb_sets:take_smallest(Fs),
    % erlang:display({Dist, Curr}),
    ?assert(Dist >= 0),
    case Curr of
        {X, Y} when ?IS_GOAL(X, Y, Tiles) ->
            Dist;
        {X, Y} ->
            {NewGs, NewFs} =
                lists:foldl(fun ({Xa, Ya} = Coord, {GsIn, FsIn} = Acc)
                                    when Xa >= 0
                                         andalso Xa < ?WIDTH * Tiles
                                         andalso Ya >= 0
                                         andalso Ya < ?HEIGHT * Tiles ->
                                    MaybeNewScore = maps:get(Curr, Gs) + edge_weight(Coord, Grid),
                                    ?assert(MaybeNewScore >= 0),
                                    case MaybeNewScore < maps:get(Coord, Gs, infinity) of
                                        true ->
                                            %% This path is better than previously known
                                            BetterScore = MaybeNewScore,
                                            GsOut = maps:put(Coord, BetterScore, GsIn),
                                            FsOut =
                                                gb_sets:add({BetterScore
                                                             + lower_bound_dist_to_goal(Coord,
                                                                                        Tiles),
                                                             Coord},
                                                            FsIn),
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

%% Tests

-ifdef(TEST).

% ex1_test() ->
%     Binary =
%         <<"1163751742\n",
%           "1381373672\n",
%           "2136511328\n",
%           "3694931569\n",
%           "7463417111\n",
%           "1319128137\n",
%           "1359912421\n",
%           "3125421639\n",
%           "1293138521\n",
%           "2311944581\n">>,
%     ?assertEqual(40, solve1(Binary)).

% ex2_test() ->
%     Binary =
%         <<"1163751742\n",
%           "1381373672\n",
%           "2136511328\n",
%           "3694931569\n",
%           "7463417111\n",
%           "1319128137\n",
%           "1359912421\n",
%           "3125421639\n",
%           "1293138521\n",
%           "2311944581\n">>,
%     ?assertEqual(2, edge_weight({10, 0}, Binary)),
%     ?assertEqual(6, edge_weight({49, 0}, Binary)),
%     ?assertEqual(6, edge_weight({0, 49}, Binary)),
%     ?assertEqual(9, edge_weight({49, 49}, Binary)),
%     ?assertEqual(3, edge_weight({1, 9}, Binary)),
%     ?assertEqual(315, solve2(Binary)).

-endif.
