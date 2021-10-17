-module(aoc2018_day17).


-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2018,
                day = 17,
                name = "Reservoir Research",
                expected = {34775, 27086},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: binary().
-type result_type() :: {any(), any()}.

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    Input.

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    Veins = veins(Input),
    Grid = grid_new(Veins),
    {MinY, MaxY} = find_y_bounds(Veins),
    {_, G0} = flow_down(Grid, starting_point(), MaxY),
    {count_reachable_tiles(G0, MinY, MaxY), count_tiles_after_draining(G0, MinY, MaxY)}.

starting_point() ->
    {500, 0}.

count_reachable_tiles(Grid, MinY, MaxY) ->
    maps:fold(fun ({_, Y}, '|', N) when (Y =< MaxY) and (Y >= MinY) ->
                      N + 1;
                  ({_, Y}, '~', N) when (Y =< MaxY) and (Y >= MinY) ->
                      N + 1;
                  ({_, _}, _, N) ->
                      N
              end,
              0,
              Grid).

count_tiles_after_draining(Grid, MinY, MaxY) ->
    maps:fold(fun ({_, Y}, '~', N) when (Y =< MaxY) and (Y >= MinY) ->
                      N + 1;
                  ({_, _}, _, N) ->
                      N
              end,
              0,
              Grid).

find_y_bounds(Veins) ->
    lists:foldl(fun ({x, _, y, {_From, To}}, {MinY, MaxY}) when To > MaxY ->
                        {MinY, To};
                    ({x, _, y, {From, _To}}, {MinY, MaxY}) when From < MinY ->
                        {From, MaxY};
                    (_, Bounds) ->
                        Bounds
                end,
                {10000000000000, 0},
                Veins).

veins(Binary) ->
    [parse_line(Line) || Line <- string:tokens(binary_to_list(Binary), "\n\r")].

parse_line(Line) ->
    parse_line0(string:tokens(Line, "=, .")).

parse_line0(["x", X, "y", From, To]) ->
    {x, list_to_integer(X), y, {list_to_integer(From), list_to_integer(To)}};
parse_line0(["y", Y, "x", From, To]) ->
    {y, list_to_integer(Y), x, {list_to_integer(From), list_to_integer(To)}}.

grid_add_vein({x, X, y, {From, To}}, Grid) ->
    lists:foldl(fun(Y, GridIn) -> GridIn#{{X, Y} => '#'} end, Grid, lists:seq(From, To));
grid_add_vein({y, Y, x, {From, To}}, Grid) ->
    lists:foldl(fun(X, GridIn) -> GridIn#{{X, Y} => '#'} end, Grid, lists:seq(From, To)).

grid_new(Veins) ->
    Grid = #{},
    grid_new(Veins, Grid).

grid_new(Veins, Grid) ->
    lists:foldl(fun grid_add_vein/2, Grid, Veins).

down({X, Y}) ->
    {X, Y + 1}.

left({X, Y}) ->
    {X - 1, Y}.

right({X, Y}) ->
    {X + 1, Y}.

%% Flow downwards from the given position
flow_down(Grid, {_X, Y}, MaxY) when Y > MaxY ->
    {flow, Grid};
flow_down(Grid, Pos, MaxY) ->
    DownPos = down(Pos),
    _From = maps:get(Pos, Grid, '.'),
    Down = maps:get(DownPos, Grid, '.'),
    case Down of
        '.' ->
            %% Empty space, continue flowing downwards
            case flow_down(maps:put(Pos, '|', Grid), DownPos, MaxY) of
                {stop, G0} ->
                    %% Flowing stopped, try flowing sideways
                    flow_sideways_leftright(G0, Pos, MaxY);
                {flow, G1} ->
                    %% We are still leaking here
                    {flow, G1}
            end;
        '|' ->
            %% Flowing down into existing flowing water body
            {flow, maps:put(Pos, '|', Grid)};
        '~' ->
            %% Flowing down into existing non-flowing water body
            %% (which has been previously filled), continue sideways
            G0 = maps:put(Pos, '|', Grid),
            flow_sideways_leftright(G0, Pos, MaxY);
        '#' ->
            %% Hit grid wall while flowing downwards, flow sideways
            G0 = maps:put(Pos, '|', Grid),
            flow_sideways_leftright(G0, Pos, MaxY)
    end.

flow_sideways_leftright(Grid, Pos, MaxY) ->
    {FlowL, G1} = flow_sideways(Grid, left(Pos), -1, MaxY),
    {FlowR, G2} = flow_sideways(G1, right(Pos), 1, MaxY),
    case {FlowL, FlowR} of
        {stop, stop} ->
            %% Stop in both directions, replace with '~'
            G3 = maps:put(Pos, '~', G2),
            G4 = flow_still_sideways(G3, left(Pos), -1),
            G5 = flow_still_sideways(G4, right(Pos), 1),
            {stop, G5};
        _ ->
            {flow, maps:put(Pos, '|', G2)}
    end.

flow_sideways(Grid, {X, Y} = Pos, Dx, MaxY) ->
    Down = maps:get(down(Pos), Grid, '.'),
    S = maps:get(Pos, Grid, '.'),

    case {Down, S} of
        %% Still water beneath
        {'~', '#'} ->
            %% Water underneath and wall sideways, stop here
            {stop, Grid};
        {'~', '~'} ->
            %% Water at rest here and below, stop
            {stop, Grid};
        {'~', '|'} ->
            %% Water flowing into water already at rest, flow sideways.
            G0 = maps:put(Pos, '|', Grid),
            flow_sideways(G0, {X + Dx, Y}, Dx, MaxY);
        {'~', '.'} ->
            %% Empty space with water beneath, continue flowing
            %% sideways
            G0 = maps:put(Pos, '|', Grid),
            flow_sideways(G0, {X + Dx, Y}, Dx, MaxY);
        %% Wall beneath
        {'#', '#'} ->
            %% Wall beneath and sideways, stop here
            {stop, Grid};
        {'#', '|'} ->
            %% Flowing into flowing water with wall beneath
            {flow, Grid};
        {'#', '.'} ->
            %% Empty space with wall beneath, continue flowing
            %% sideways
            G0 = maps:put(Pos, '|', Grid),
            flow_sideways(G0, {X + Dx, Y}, Dx, MaxY);
        %% Flowing water beneat
        {'|', '|'} ->
            %% Flowing into flowing water with flowing water beneath
            {flow, Grid};
        {'|', '.'} ->
            %% Flowing into empty space with flowing water beneath
            {flow, maps:put(Pos, '|', Grid)};
        %% Empty space beneat
        {'.', '.'} ->
            %% No space below, flow downwards
            G0 = maps:put(Pos, '|', Grid),
            case flow_down(G0, down(Pos), MaxY) of
                {stop, G1} ->
                    %% We filled a body of water while flowing downwards,
                    %% so we need to continue filling sideways
                    flow_sideways(G1, {X + Dx, Y}, Dx, MaxY);
                {flow, G1} ->
                    %% Continue flowing.
                    {flow, G1}
            end
    end.

flow_still_sideways(Grid, {X, Y} = Pos, Dx) ->
    case maps:get(Pos, Grid, false) of
        '|' ->
            G0 = maps:put(Pos, '~', Grid),
            flow_still_sideways(G0, {X + Dx, Y}, Dx);
        _ ->
            Grid
    end.
