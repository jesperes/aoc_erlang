%%% Advent of Code solution for 2019 day 10.
%%% Created: 2019-12-10T05:33:20+00:00

-module(aoc2019_day10).

-include_lib("stdlib/include/assert.hrl").

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 10,
                name = "Monitoring Station",
                expected = {334, 1119},
                use_one_solver_fun = true,
                has_input_file = true}.

-type asteroid() :: {X :: integer(), Y :: integer()}.
-type input_type() :: [asteroid()].
-type result_type() :: {integer(), integer()}.

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    %% Grid is 34 chars wide (+ newline)
    parse_grid(Binary, 34).

-spec solve(Input :: input_type()) -> result_type().
solve(Asteroids) ->
    %% Compute a list of {A, B, Dir} where A and B are asteroid coords
    %% and Dir is the direction from A to B expressed as a fraction.
    AsteroidDirs = [{A, B, direction(A, B)} || A <- Asteroids, B <- Asteroids, A =/= B],

    %% Compute a map where the keys are asteroids (A). Each key maps to
    %% a map of (integer() -> list(Asteroids)), where the integer is the
    %% slope (direction), and the list contains all the asteroids which
    %% lie along that line.
    AsteroidsGroupedByVisibility =
        lists:foldl(fun({A, B, Dir}, Map) ->
                       Dist = distance(A, B),
                       maps:update_with(A,
                                        fun(OldMap) ->
                                           maps:update_with(Dir,
                                                            fun(OldList) -> [{Dist, B} | OldList]
                                                            end,
                                                            [{Dist, B}],
                                                            OldMap)
                                        end,
                                        #{Dir => [{Dist, B}]},
                                        Map)
                    end,
                    #{},
                    AsteroidDirs),

    %% Find answer to part 1: the asteroids where we can see most other
    %% asteroids.
    {_, Part1, VisibleAsteroids} =
        maps:fold(fun(K, V, {_, Max, _} = Acc) ->
                     case maps:size(V) of
                         Len when Len > Max -> {K, Len, V};
                         _ -> Acc
                     end
                  end,
                  {undef, 0, []},
                  AsteroidsGroupedByVisibility),

    %% Find out which asteroids we should fire at, and in which order.
    %% Part 2 solution is to find the 200th destroyed
    %% asteroid. Fortunately, we only need to fire at the closest
    %% asteroids; as 200 < 334 (which is the number of visible
    %% asteroids.
    FiringOrder =
        lists:map(fun(Dir) ->
                     lists:min(
                         maps:get(Dir, VisibleAsteroids))
                  end,
                  lists:sort(
                      maps:keys(VisibleAsteroids))),

    Part2 = fire_ze_huge_lazer(FiringOrder, 1),
    {Part1, Part2}.

fire_ze_huge_lazer([], _) ->
    not_enough_asteroids; %% This should only happen with tests
fire_ze_huge_lazer([{_, {X, Y}} | Orders], N) ->
    case N of
        200 ->
            X * 100 + Y;
        _ ->
            fire_ze_huge_lazer(Orders, N + 1)
    end.

%% ---- [ Helpers ] ----

distance({X0, Y0}, {X1, Y1}) ->
    math:sqrt(math:pow(X1 - X0, 2) + math:pow(Y1 - Y0, 2)).

%% When using floats as keys, we need to make sure there are no
%% rounding errors.
f_to_i(F) ->
    floor(F * 1000000).

%% Return the angle (in radians) of the vector {X0,Y0} -> {X1, Y1}.
direction({X0, Y0}, {X1, Y1}) ->
    Dx = X1 - X0,
    Dy = Y1 - Y0,
    Pi = 3.141592653589793,
    %% This yuck is to get an angle where 0 is north, and increases
    %% clock-wise.
    Z = math:atan2(-Dy, Dx) + 3 * Pi / 2,
    Z0 = if Z > 2 * Pi ->
                Z - 2 * Pi;
            true ->
                Z
         end,
    f_to_i(2 * Pi - Z0).

-spec parse_grid(binary(), Width :: integer()) -> [asteroid()].
parse_grid(Binary, Width) ->
    ?assertEqual($\n, binary:at(Binary, Width)),
    lists:map(fun({Offset, _} = _Match) -> {Offset rem (Width + 1), Offset div (Width + 1)}
              end,
              binary:matches(Binary, <<"#">>)).
