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

% Code the entries in the open set into a single integer instead of a {Dist, {X,
% Y}} tuple. This seems to gain a bit in performance.
-define(MAKE_KEY(Dist, X, Y), Dist bsl 32 bor (X band 16#ffff bsl 16) bor Y band 16#ffff).
-define(EXTRACT_KEY(F), {F bsr 32, {(F bsr 16) band 16#ffff, F band 16#ffff}}).

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
    find(#{Start => 0}, % actual cost to position
         gb_sets:singleton(?MAKE_KEY(lower_bound_dist_to_goal(Start, Tiles), 0, 0)),
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
    {F, Fs0} = gb_sets:take_smallest(Fs),
    {Dist, Curr} = ?EXTRACT_KEY(F),
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
                                    MaybeNewScore = maps:get(Curr, GsIn) + edge_weight(Coord, Grid),
                                    case MaybeNewScore < maps:get(Coord, GsIn, infinity) of
                                        true ->
                                            %% This path is better than previously known
                                            BetterScore = MaybeNewScore,
                                            GsOut = maps:put(Coord, BetterScore, GsIn),
                                            NewDist =
                                                BetterScore
                                                + lower_bound_dist_to_goal(Coord, Tiles),
                                            FKey = ?MAKE_KEY(NewDist, Xa, Ya),
                                            FsOut = gb_sets:add(FKey, FsIn),
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
         F = ?MAKE_KEY(Dist, X, Y),
         Out = ?EXTRACT_KEY(F),
         ?assertEqual(Out, {Dist, {X, Y}})
     end
     || Dist <- lists:seq(1000, 1100), X <- lists:seq(500, 510), Y <- lists:seq(500, 600, 10)].

-endif.
