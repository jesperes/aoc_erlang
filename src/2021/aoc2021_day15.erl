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
                expected = {0, 0},
                has_input_file = true}.

-type input_type() :: any().
-type result_type() :: integer().

-define(WIDTH, 100).
-define(HEIGHT, 100).
-define(IS_GOAL(X, Y), X == ?WIDTH - 1 andalso Y == ?HEIGHT - 1).
-define(DELTAS, [{-1, 0}, {0, -1}, {1, 0}, {0, 1}]).

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    Binary.

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    find(Input).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    find(Input).

find(Input) ->
    Start = {0, 0},
    find(#{Start => 0}, % actual cost to position
         gb_sets:singleton({heuristic(Start), Start}),
         Input).

%% Cost of moving to a coordinate.
edge_weight({X, Y}, Grid) ->
    binary:at(Grid, Y * (?WIDTH + 1) + X) - $0.

heuristic({X, Y}) ->
    abs(?WIDTH - 1 - X) + abs(?HEIGHT- 1 - Y).

%% A* implementation
find(Gs, Fs, Grid) ->
    {{Dist, Curr}, Fs0} = gb_sets:take_smallest(Fs),
    case Curr of
        {X, Y} when ?IS_GOAL(X, Y) ->
            Dist;
        {X, Y} ->
            {NewGs, NewFs} =
                lists:foldl(fun ({Xa, Ya} = Coord, {GsIn, FsIn} = Acc)
                                    when Xa >= 0
                                         andalso Xa < ?WIDTH
                                         andalso Ya >= 0
                                         andalso Ya < ?HEIGHT ->
                                    MaybeNewScore =
                                        maps:get(Curr, Gs) + edge_weight(Coord, Grid),
                                    case MaybeNewScore < maps:get(Coord, Gs, infinity) of
                                        true ->
                                            %% This path is better than previously known
                                            GsOut = maps:put(Coord, MaybeNewScore, GsIn),
                                            FsOut =
                                                gb_sets:add({MaybeNewScore + heuristic(Coord),
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
            find(NewGs, NewFs, Grid)
    end.

%% Tests

-ifdef(TEST).

ex1_test() ->
    Binary =
        <<"1163751742\n",
          "1381373672\n",
          "2136511328\n",
          "3694931569\n",
          "7463417111\n",
          "1319128137\n",
          "1359912421\n",
          "3125421639\n",
          "1293138521\n",
          "2311944581\n">>,
    ?assertEqual(40, solve1(Binary)).

-endif.
