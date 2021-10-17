-module(aoc2016_day15).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 15,
                name = "Timing Is Everything",
                expected = {400589, 3045959},
                has_input_file = false}.

-type input_type() :: [{integer(), {integer(), integer()}}].
-type result1_type() :: any().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    Binary =
        <<"Disc #1 has 17 positions; at time=0, it is at position 15.\n",
          "Disc #2 has 3 positions; at time=0, it is at position 2.\n",
          "Disc #3 has 19 positions; at time=0, it is at position 4.\n",
          "Disc #4 has 13 positions; at time=0, it is at position 2.\n",
          "Disc #5 has 7 positions; at time=0, it is at position 2.\n",
          "Disc #6 has 5 positions; at time=0, it is at position 0.\n",
          "Disc #7 has 11 positions; at time=0, it is at position 0.\n">>,
    Lines = string:tokens(binary_to_list(Binary), "\n\r"),
    lists:map(fun parse_line/1, Lines).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    %% Last element in the input list is only used for part 2.
    find_earliest_release(maps:from_list(
                              lists:sublist(Input, 6)),
                          7).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    find_earliest_release(maps:from_list(Input), 8).

simulate(_Discs, Level, _DT, TargetLevel) when Level == TargetLevel ->
    ok;
simulate(Discs, Level, DT, TargetLevel) ->
    case maps:get(Level, Discs, falling) of
        falling ->
            simulate(Discs, Level + 1, DT, TargetLevel);
        {NumPos, InitPos} ->
            if (InitPos + DT + Level) rem NumPos == 0 ->
                   simulate(Discs, Level + 1, DT, TargetLevel);
               true ->
                   lost
            end
    end.

find_earliest_release(Discs, TL) ->
    find_earliest_release(Discs, 0, TL).

find_earliest_release(Discs, T, TL) ->
    case simulate(Discs, 0, T, TL) of
        ok ->
            T;
        lost ->
            find_earliest_release(Discs, T + 1, TL)
    end.

%%% Parser

ltoi(N) ->
    list_to_integer(N).

parse_line(Line) ->
    ["Disc",
     Disc,
     "has",
     N,
     "positions",
     "at",
     "time",
     _Time,
     "it",
     "is",
     "at",
     "position",
     Pos] =
        string:tokens(Line, " ;,.#="),
    {ltoi(Disc), {ltoi(N), ltoi(Pos)}}.
