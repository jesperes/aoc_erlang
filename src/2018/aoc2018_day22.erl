-module(aoc2018_day22).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2018,
                day = 22,
                name = "Mode Maze",
                expected = {6256, 973},
                has_input_file = false}.

-type input_type() :: map().
-type result_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    Depth = 5913,
    Target = {8, 701, 1},
    compute_grid(150, 800, Depth, Target).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Grid) ->
    {Xt, Yt, _} = maps:get(target, Grid),
    Coords = [{X, Y} || X <- lists:seq(0, Xt), Y <- lists:seq(0, Yt)],
    Fun = fun(Pos, N) ->
             {_, RT} = maps:get(Pos, Grid),
             RT + N
          end,
    lists:foldl(Fun, 0, Coords).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Grid) ->
    Goal = {Xt, Yt, Tt} = maps:get(target, Grid),
    Start = {0, 0, 1}, %% 1 == torch

    CostFn =
        fun({X, Y, Tool}) ->
           C = abs(X - Xt) + abs(Y - Yt),
           if Tt == Tool -> C;
              true -> C + 7
           end
        end,

    NbrFn =
        fun(Node) ->
           Nbrs = nbrs(Node, Grid),
           %% erlang:display({nbrs, Nbrs}),
           Nbrs
        end,

    DistFn =
        fun({_, _, Tool1}, {_, _, Tool2}) ->
           if Tool1 == Tool2 -> 1;
              true -> 8
           end
        end,

    {Dist, _Path} = astar2:astar(Start, Goal, CostFn, NbrFn, DistFn),
    %% lists:foreach(fun(P) ->
    %%                           io:format("Step: ~w~n", [P])
    %%                    end, lists:reverse(Path)),
    Dist.

compute_grid_cell(X, Grid) ->
    Y = maps:get(y, Grid),
    Depth = maps:get(depth, Grid),
    {Xt, Yt, _} = maps:get(target, Grid),
    GI = case {X, Y} of
             {X, 0} ->
                 X * 16807;
             {0, Y} ->
                 Y * 48271;
             {Xt, Yt} ->
                 0;
             {X, Y} ->
                 {EL1, _} = maps:get({X, Y - 1}, Grid),
                 {EL2, _} = maps:get({X - 1, Y}, Grid),
                 EL1 * EL2
         end,
    EL = (GI + Depth) rem 20183,
    RT = EL rem 3,
    Grid#{{X, Y} => {EL, RT}}.

compute_grid_line(Y, #{max_x := MaxX} = Grid) ->
    lists:foldl(fun compute_grid_cell/2, Grid#{y => Y}, lists:seq(0, MaxX)).

compute_grid(MaxX, MaxY, Depth, Target) ->
    lists:foldl(fun compute_grid_line/2,
                #{max_x => MaxX,
                  depth => Depth,
                  target => Target},
                lists:seq(0, MaxY)).

%% We represent the tools as [0, 1, 2] and the region type as [0, 1,
%% 2]. With (climbing gear == 0, torch == 1, and 2 == neither) and (0
%% == rocky, 1 == wet, 2 == narrow), checking for matching region type
%% with tool simply becomes Tool != RegionType.

is_valid_tool(Pos, T, Grid) ->
    {_, RT} = maps:get(Pos, Grid),
    T =/= RT.

filter_nbr({{X, Y}, T}, {Xn, Yn, _}, Grid, Acc) when (X >= 0) and (Y >= 0) ->
    %% Note that the tool needs to be valid both in the current
    %% location and in the location we are moving to, because we need
    %% to change first, then move.
    case is_valid_tool({X, Y}, T, Grid) and is_valid_tool({Xn, Yn}, T, Grid) of
        true ->
            [{X, Y, T} | Acc];
        false ->
            Acc
    end;
filter_nbr(_, _, _, Acc) ->
    Acc.

%% Return the list of neighbors to {X, Y, T}.
nbrs({X, Y, _T} = Node, Grid) ->
    lists:foldl(fun(Nbr, Acc) -> filter_nbr(Nbr, Node, Grid, Acc) end,
                [],
                [{Pos, Tool}
                 || Tool <- [0, 1, 2], Pos <- [{X - 1, Y}, {X + 1, Y}, {X, Y + 1}, {X, Y - 1}]]).
