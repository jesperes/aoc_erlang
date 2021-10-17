-module(aoc2015_day18).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 18,
                name = "Like a GIF For Your Yard",
                expected = {768, 781},
                has_input_file = true}.

-type input_type() :: binary().
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    Input.

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    run(Input, false).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    run(Input, true).

run(Binary, CornersAlwaysOn) ->
    Bounds = {100, 100},
    Grid = input(Binary, 101, Bounds, CornersAlwaysOn),
    G0 = lists:foldl(fun(_N, G) -> next_state(G, Bounds, CornersAlwaysOn) end,
                     Grid,
                     lists:seq(1, 100)),
    maps:size(G0).

match({Pos, _}, Width) ->
    {Pos rem Width, Pos div Width}.

input(Binary, Width, Bounds, CornersAlwaysOn) ->
    maps:from_list([{match(Match, Width), true} || Match <- binary:matches(Binary, <<"#">>)]
                   ++ corners(Bounds, CornersAlwaysOn)).

corners(_, false) ->
    [];
corners({MaxX, MaxY}, true) ->
    [{0, 0}, {MaxX - 1, 0}, {0, MaxY - 1}, {MaxX - 1, MaxY - 1}].

adjacent({X, Y}, {MaxX, MaxY}) ->
    [{Xa, Ya}
     || Xa <- lists:seq(X - 1, X + 1),
        Ya <- lists:seq(Y - 1, Y + 1),
        {Xa, Ya} /= {X, Y},
        Xa >= 0,
        Ya >= 0,
        Xa < MaxX,
        Ya < MaxY].

next_state(Grid, {MaxX, MaxY} = Bounds, CornersAlwaysOn) ->
    Xs = [{{X, Y}, next_state0({X, Y}, Grid, Bounds, CornersAlwaysOn)}
          || X <- lists:seq(0, MaxX - 1), Y <- lists:seq(0, MaxY - 1)],
    lists:foldl(fun ({Pos, true}, S) ->
                        maps:put(Pos, true, S);
                    (_, S) ->
                        S
                end,
                #{},
                Xs).

next_state0(Pos, Grid, Bounds, CornersAlwaysOn) ->
    NumAdjOn =
        length(lists:filter(fun(Adj) -> maps:is_key(Adj, Grid) end, adjacent(Pos, Bounds))),

    IsCorner = lists:member(Pos, corners(Bounds, CornersAlwaysOn)),
    IsOn = maps:is_key(Pos, Grid),

    case {IsCorner, IsOn, NumAdjOn} of
        {true, _, _} ->
            true;
        {_, true, 2} ->
            true;
        {_, true, 3} ->
            true;
        {_, true, _} ->
            false;
        {_, false, 3} ->
            true;
        {_, false, _} ->
            false
    end.
