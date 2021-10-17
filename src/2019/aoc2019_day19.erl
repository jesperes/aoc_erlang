-module(aoc2019_day19).

-behavior(aoc_puzzle).

-include_lib("stdlib/include/assert.hrl").

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 19,
                name = "Tractor Beam",
                expected = {164, 13081049},
                has_input_file = true}.

-type input_type() :: intcode:intcode_program().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    intcode:parse(Binary).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Prog) ->
    lists:sum([read_point(X, Y, Prog) || X <- lists:seq(0, 49), Y <- lists:seq(0, 49)]).

%% For part two we need to find the closest point where the 100x100
%% spaceship fits entirely within the tractor beam. This turned out to
%% be most easily done by trial-and-error.
-spec solve2(Input :: input_type()) -> result_type().
solve2(Prog) ->
    Size = 100,
    X = 1308,
    Y = 1049,
    ?assert(fits(Prog, X, Y, Size)),
    X * 10000 + Y.

%% Returns 1 if {X,Y} is covered by the tractor beam, 0 otherwise.
read_point(X, Y, Prog) ->
    {_, [R]} = intcode:execute(Prog, [X, Y]),
    R.

fits(Prog, X, Y, Size) ->
    S1 = lists:sum([read_point(X, Y + Y0, Prog) || Y0 <- lists:seq(0, 99)]),
    S2 = lists:sum([read_point(X + X0, Y, Prog) || X0 <- lists:seq(0, 99)]),
    S1 >= Size andalso S2 >= Size.
