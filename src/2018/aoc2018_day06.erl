-module(aoc2018_day06).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-define(TIE, '.').

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2018,
                day = 6,
                name = "Chronal Coordinates",
                expected = {3894, 39398},
                has_input_file = true}.

-type input_type() :: [{integer(), integer()}].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    Points =
        lists:map(fun(Str) ->
                     [X, Y] = string:tokens(Str, ", "),
                     {list_to_integer(X), list_to_integer(Y)}
                  end,
                  string:tokens(binary_to_list(Input), "\n\r")),
    {List, _} = lists:mapfoldl(fun(Str, Acc) -> {{Acc, Str}, Acc + 1} end, 0, Points),
    List.

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Points) ->
    Limits = {MinX, MaxX, MinY, MaxY} = find_grid_limits(Points),

    Grid =
        [[assign_grid_color({X, Y}, Points, Limits) || X <- lists:seq(MinX, MaxX)]
         || Y <- lists:seq(MinY, MaxY)],

    FlatGrid = lists:flatten(Grid),

    InfiniteAreas = get_infinite_areas(FlatGrid, sets:new()),

    FiniteGridCells =
        lists:filter(fun({X, _}) -> not sets:is_element(X, InfiniteAreas) end, FlatGrid),

    FiniteAreas = compute_areas(FiniteGridCells, #{}),
    get_largest_area(FiniteAreas).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Points) ->
    Limit = 10000,

    {MinX, MaxX, MinY, MaxY} = find_grid_limits(Points),

    SumDistances =
        [[sum_distance_to_all_points({X, Y}, Points) || X <- lists:seq(MinX, MaxX)]
         || Y <- lists:seq(MinY, MaxY)],

    length(lists:filter(fun(Dist) -> Dist < Limit end, lists:flatten(SumDistances))).

get_largest_area(Cells) ->
    maps:fold(fun (_K, V, CurrMax) when V >= CurrMax ->
                      V;
                  (_K, _V, AccIn) ->
                      AccIn
              end,
              0,
              Cells).

get_infinite_areas([], Set) ->
    Set;
get_infinite_areas([{N, true} | Grid], Set) ->
    get_infinite_areas(Grid, sets:add_element(N, Set));
get_infinite_areas([{_, false} | Grid], Set) ->
    get_infinite_areas(Grid, Set).

incr(N) ->
    N + 1.

compute_areas([], Areas) ->
    Areas;
compute_areas([{?TIE, _} | Grid], Areas) ->
    compute_areas(Grid, Areas);
compute_areas([{N, _} | Grid], Areas) ->
    compute_areas(Grid, maps:update_with(N, fun incr/1, 1, Areas)).

assign_grid_color(P, Points, Limits) ->
    %% Compute the manhattan distances from P to all
    %% the points in "Points".
    Color =
        case lists:sort(
                 lists:map(fun({Id, {Px, Py}}) -> {manhattan(P, {Px, Py}), Id} end, Points))
        of
            [{X, _Id1}, {X, _Id2} | _] ->
                %% Point is closest to both Id1 and Id2
                ?TIE;
            [{_X, Id} | _] ->
                Id
        end,

    OnEdge = is_on_edge(P, Limits),
    {Color, OnEdge}.

is_on_edge({X, _Y}, {X, _MaxX, _MinY, _MaxY}) ->
    true;
is_on_edge({X, _Y}, {_MinX, X, _MinY, _MaxY}) ->
    true;
is_on_edge({_X, Y}, {_MinX, _MaxX, Y, _MaxY}) ->
    true;
is_on_edge({_X, Y}, {_MinX, _MaxX, _MinY, Y}) ->
    true;
is_on_edge(_, _) ->
    false.

find_grid_limits(Points) ->
    find_grid_limits(Points, {undef, undef, undef, undef}).

find_grid_limits([], Limits) ->
    Limits;
find_grid_limits([{_Id, {X, Y}} | Ps], {MinX, MaxX, MinY, MaxY}) ->
    NewMinX = get_new_min(X, MinX),
    NewMaxX = get_new_max(X, MaxX),
    NewMinY = get_new_min(Y, MinY),
    NewMaxY = get_new_max(Y, MaxY),
    find_grid_limits(Ps, {NewMinX, NewMaxX, NewMinY, NewMaxY}).

get_new_min(N, OldMin) when OldMin =:= undef ->
    N;
get_new_min(N, OldMin) when N =< OldMin ->
    N;
get_new_min(_, OldMin) ->
    OldMin.

get_new_max(N, OldMax) when OldMax =:= undef ->
    N;
get_new_max(N, OldMax) when N >= OldMax ->
    N;
get_new_max(_, OldMax) ->
    OldMax.

manhattan({X0, Y0} = _From, {X1, Y1} = _To) ->
    abs(X0 - X1) + abs(Y0 - Y1).

sum_distance_to_all_points(_, []) ->
    0;
sum_distance_to_all_points(P1, [{_, P2} | Points]) ->
    manhattan(P1, P2) + sum_distance_to_all_points(P1, Points).
