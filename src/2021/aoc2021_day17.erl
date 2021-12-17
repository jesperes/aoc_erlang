-module(aoc2021_day17).

-include_lib("eunit/include/eunit.hrl").

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 17,
                name = "Trick Shot",
                expected = {30628, 4433},
                has_input_file = false,
                use_one_solver_fun = true}.

-type input_type() ::
    {{MinX :: integer(), MaxX :: integer()}, {MinY :: integer(), MaxY :: integer()}}.
-type result_type() :: {integer(), integer()}.

-spec parse(Binary :: binary()) -> input_type().
parse(_Binary) ->
    {{29, 73}, {-248, -194}}.

-spec solve(Input :: input_type()) -> result_type().
solve(Target) ->
    % Part 1: find the velocity with the given max Y pos.
    %
    % Part 2: find how many distinct velocities hit the target area.
    %
    % The parameters used here are an estimate of the actual range of initial
    % velocities which will hit the target area.
    Hits = try_fire_probe(Target, 5, 80, -250, 250),

    {lists:max(Hits), length(Hits)}.

try_fire_probe(Target, MinX, MaxX, MinY, MaxY) ->
    Vs = [{X, Y} || X <- lists:seq(MinX, MaxX), Y <- lists:seq(MinY, MaxY)],
    lists:filtermap(fun(V) -> trajectory(V, Target) end, Vs).

inside_target_area({X, Y}, {{XMin, XMax}, {YMin, YMax}}) ->
    X >= XMin andalso X =< XMax andalso Y >= YMin andalso Y =< YMax.

%% Probe has passed target area if it is below it and
%% moving downwards.
past_target_area({_X, Y}, {_Dx, Dy}, {_, {YMin, _}}) when Dy < 0 andalso Y < YMin ->
    true;
past_target_area(_, _, _) ->
    false.

trajectory(Velocity, Target) ->
    trajectory(Velocity, {0, 0}, Target, -10000).

trajectory({Dx, Dy} = V, {X, Y} = Pos, Target, MaxY) ->
    case {inside_target_area(Pos, Target), past_target_area(Pos, V, Target)} of
        {true, _} ->
            {true, MaxY};
        {_, true} ->
            false;
        _ ->
            X0 = X + Dx,
            Y0 = Y + Dy,
            Dx0 = drag_x(Dx),
            Dy0 = Dy - 1,
            MaxY0 = max(Y0, MaxY),
            trajectory({Dx0, Dy0}, {X0, Y0}, Target, MaxY0)
    end.

drag_x(X) when X > 0 ->
    X - 1;
drag_x(X) when X < 0 ->
    X + 1;
drag_x(X) ->
    X.
