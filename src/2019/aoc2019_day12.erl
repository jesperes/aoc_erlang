%%% Advent of Code solution for 2019 day 12.
%%% Created: 2019-12-12T05:22:58+00:00

-module(aoc2019_day12).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 12,
                name = "The N-Body Problem",
                expected = {8362, 478373365921244},
                has_input_file = false}.

-type coordinate() :: {X :: integer(), Y :: integer(), Z :: integer()}.
-type input_type() :: [{coordinate(), coordinate()}].
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(_Binary) ->
    [{{10, 15, 7}, {0, 0, 0}},
     {{15, 10, 0}, {0, 0, 0}},
     {{20, 12, 3}, {0, 0, 0}},
     {{0, -3, 13}, {0, 0, 0}}].

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    part1(Input, 1000).

%% This answer is slightly less than 500 trillion, or 4.78 *
%% 10^14.  In part 1, it takes us ~ 3 microseconds/iteration. At
%% that speed, computing part 2 would take us ~45 years, and that
%% does not take into account the enormous amount of memory needed
%% to store all the states.
-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    part2(Input).

%% Puzzle solution
part1(Bodies, Steps) ->
    FinalBodies = nbodies(Bodies, Steps),
    energy(FinalBodies).

nbodies(Bodies, 0) ->
    Bodies;
nbodies(Bodies, N) ->
    nbodies(do_one_step(Bodies), N - 1).

do_one_step(Bodies) ->
    [velocity(B) || B <- apply_gravity(Bodies)].

apply_gravity(Bodies) ->
    [lists:foldl(fun (A, A) ->
                         A;
                     (Other, Acc) ->
                         gravity(Acc, Other)
                 end,
                 B,
                 Bodies)
     || B <- Bodies].

gravity({{X, Y, Z} = Pos, {Dx, Dy, Dz}}, {{Xo, Yo, Zo}, _}) ->
    {Pos, {Dx + sign(Xo - X), Dy + sign(Yo - Y), Dz + sign(Zo - Z)}}.

velocity({{X, Y, Z}, {Dx, Dy, Dz} = D}) ->
    {{X + Dx, Y + Dy, Z + Dz}, D}.

sign(X) when X < 0 ->
    -1;
sign(X) when X == 0 ->
    0;
sign(_) ->
    1.

energy(Bodies) ->
    lists:foldl(fun({{X, Y, Z}, {Dx, Dy, Dz}}, Acc) ->
                   PE = abs(X) + abs(Y) + abs(Z),
                   KE = abs(Dx) + abs(Dy) + abs(Dz),
                   Acc + PE * KE
                end,
                0,
                Bodies).

%% Part 2
part2(Bodies) ->
    States = [sets:new(), sets:new(), sets:new()],
    run2(Bodies, States, 0).

run2(Bodies, States, N) ->
    States0 = check_cycles(Bodies, States, N),

    case lists:all(fun is_integer/1, States0) of
        true ->
            lists:foldl(fun lcm/2, 1, States0);
        false ->
            Bodies0 = do_one_step(Bodies),
            run2(Bodies0, States0, N + 1)
    end.

%% Key observation: since the coordinates are independent, we can detect
%% cycles in each coordinate individually, then use LCM to compute the
%% period of the entire system.
check_cycles(Bodies, [Sx, Sy, Sz], N) ->
    {Xcoords, Ycoords, Zcoords} = get_coords(Bodies, [], [], []),
    [update_cycle(Xcoords, Sx, N),
     update_cycle(Ycoords, Sy, N),
     update_cycle(Zcoords, Sz, N)].

%% Take a list of bodies, and return a three-tuple containing X, Y,
%% and Z states respectively.
get_coords([], AccX, AccY, AccZ) ->
    {AccX, AccY, AccZ};
get_coords([{{X, Y, Z}, {Dx, Dy, Dz}} | Rest], AccX, AccY, AccZ) ->
    get_coords(Rest, [{X, Dx} | AccX], [{Y, Dy} | AccY], [{Z, Dz} | AccZ]).

update_cycle(Coords, Set, N) ->
    case sets:is_set(Set) of
        false ->
            Set;
        true ->
            case sets:is_element(Coords, Set) of
                true ->
                    N;
                false ->
                    sets:add_element(Coords, Set)
            end
    end.

gcd(A, B) when A == 0; B == 0 ->
    0;
gcd(A, B) when A == B ->
    A;
gcd(A, B) when A > B ->
    gcd(A - B, B);
gcd(A, B) ->
    gcd(A, B - A).

lcm(A, B) ->
    A * B div gcd(A, B).
