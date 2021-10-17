-module(aoc2016_day13).

-behavior(aoc_puzzle).

%% dist/2 is used as a search callback, but doesn't use its arguments.
-hank([{unnecessary_function_arguments, [{distance, 2}]}]).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-define(START, {1, 1}).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 13,
                name = "A Maze of Twisty Little Cubicles",
                expected = {86, 127},
                has_input_file = false}.

-type input_type() :: integer().
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    1364.

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Fav) ->
    Goal = {31, 39},

    {_, Path} =
        astar2:astar(?START,
                     Goal,
                     fun(Node) -> cost(Node, Goal) end,
                     fun(Curr) -> neighbors(Curr, Fav) end,
                     fun distance/2),
    length(Path) - 1.

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Fav) ->
    sets:size(search_to_depth(?START, 50, Fav)).

bitcount(V) ->
    bitcount(V, 0).

bitcount(0, C) ->
    C;
bitcount(V, C) ->
    bitcount(V band (V - 1), C + 1).

is_wall({X, Y}, Fav) ->
    bitcount(X * X + 3 * X + 2 * X * Y + Y + Y * Y + Fav) rem 2 == 1.

%% A* search algorithm callbacks
distance(_, _) ->
    1.

cost({X0, Y0}, {X1, Y1}) ->
    abs(X0 - X1) + abs(Y0 - Y1).

neighbors({X, Y}, Fav) ->
    lists:filter(fun({X0, Y0} = Pos) -> (X0 >= 0) and (Y0 >= 0) and not is_wall(Pos, Fav) end,
                 [{X - 1, Y}, {X + 1, Y}, {X, Y + 1}, {X, Y - 1}]).

%% Part 2: enumerate all positions reachable using max 50 steps.  Use
%% a depth-first search, but keep track of the depth at which each
%% node was found so we can revisit nodes when seen at an earlier
%% depth.
search_to_depth(Start, Depth, Fav) ->
    Visited = search_to_depth(Start, 1, Depth, Fav, #{Start => 0}),
    sets:from_list(
        maps:keys(Visited)).

search_to_depth(_, CurrDepth, MaxDepth, _Fav, Visited) when CurrDepth > MaxDepth ->
    Visited;
search_to_depth(Curr, CurrDepth, MaxDepth, Fav, Visited) ->
    lists:foldl(fun(Nbr, VisitedIn) ->
                   case maps:get(Nbr, VisitedIn, inf) of
                       NbrDepth when NbrDepth > CurrDepth ->
                           search_to_depth(Nbr,
                                           CurrDepth + 1,
                                           MaxDepth,
                                           Fav,
                                           VisitedIn#{Nbr => CurrDepth});
                       _ -> VisitedIn
                   end
                end,
                Visited,
                neighbors(Curr, Fav)).
