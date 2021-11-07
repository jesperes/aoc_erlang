-module(aoc2017_day20).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 20,
                name = "Particle Swarm",
                expected = {457, 448},
                has_input_file = true}.

-type coord() :: {integer(), integer(), integer()}.

-record(particle,
        {id :: integer() | undefined,
         p :: coord(),
         v :: coord() | undefined,
         a :: coord() | undefined}).

-type particle() :: #particle{}.
-type input_type() :: [particle()].
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    Lines = string:tokens(binary_to_list(Binary), "\n\r"),
    lists:map(fun({Id, Line}) ->
                 [Px, Py, Pz, Vx, Vy, Vz, Ax, Ay, Az] =
                     lists:map(fun erlang:list_to_integer/1, string:tokens(Line, "pva=<>, ")),
                 #particle{id = Id,
                           p = {Px, Py, Pz},
                           v = {Vx, Vy, Vz},
                           a = {Ax, Ay, Az}}
              end,
              lists:zip(
                  lists:seq(0, length(Lines) - 1), Lines)).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Particles) ->
    {_, _, _, Id} =
        hd(lists:sort(
               lists:map(fun(P) ->
                            {manhattan(P#particle.a),
                             manhattan(P#particle.v),
                             manhattan(P#particle.p),
                             P#particle.id}
                         end,
                         Particles))),
    Id.

-spec solve2(Input :: input_type()) -> result_type().
solve2(Particles) ->
    %% We happen to know that 40 is the smallest number of iterations needed
    %% to arrive at the correct solution.
    length(remove_colliding_particles(Particles, 40)).

remove_colliding_particles(Particles, 0) ->
    Particles;
remove_colliding_particles(Particles, N) ->
    NoDups = remove_duplicates(Particles),
    Next = step_all(NoDups),
    remove_colliding_particles(Next, N - 1).

remove_duplicates(Particles) ->
    {_, Dups} =
        lists:foldl(fun(#particle{p = Pos}, {Seen, Dups}) ->
                       case gb_sets:is_element(Pos, Seen) of
                           true -> {Seen, gb_sets:add_element(Pos, Dups)};
                           false -> {gb_sets:add_element(Pos, Seen), Dups}
                       end
                    end,
                    {gb_sets:new(), gb_sets:new()},
                    Particles),
    DupList = gb_sets:to_list(Dups),
    lists:filter(fun(P) -> not lists:member(P#particle.p, DupList) end, Particles).

step_all([]) ->
    [];
step_all([P | Particles]) ->
    A = {Ax, Ay, Az} = P#particle.a,
    {Vx, Vy, Vz} = P#particle.v,
    {Px, Py, Pz} = P#particle.p,
    NewV = {NewVx, NewVy, NewVz} = {Vx + Ax, Vy + Ay, Vz + Az},
    NewP = {Px + NewVx, Py + NewVy, Pz + NewVz},
    [#particle{p = NewP,
               v = NewV,
               a = A}
     | step_all(Particles)].

manhattan({Ax, Ay, Az}) ->
    abs(Ax) + abs(Ay) + abs(Az).
