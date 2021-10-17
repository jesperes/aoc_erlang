-module(aoc2016_day18).

-include("aoc_puzzle.hrl").

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-define(TRAP, $^).
-define(SAFE, $.).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 18,
                name = "Like A Rogue",
                expected = {2035, 20000577},
                has_input_file = false}.

-type input_type() :: string().
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    ".^..^....^....^^.^^.^.^^.^.....^.^..^...^^^^^^.^^^^.^.^^^^^^^.^^^^^."
    ".^.^^^.^^..^.^^.^....^.^...^^.^.".

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    count_tiles(Input, 40).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    count_tiles(Input, 400 * 1000).

%%% Implementation

next_row([Center, Right | Row]) ->
    next_row(?SAFE, Center, Right, Row).

next_row(Left, Center, Right, []) ->
    [is_trap(Left, Center, Right), is_trap(Center, Right, ?SAFE)];
next_row(Left, Center, Right, [NextRight | Rest]) ->
    Trap = is_trap(Left, Center, Right),
    [Trap | next_row(Center, Right, NextRight, Rest)].

is_trap(?TRAP, ?TRAP, ?SAFE) ->
    ?TRAP;
is_trap(?SAFE, ?TRAP, ?TRAP) ->
    ?TRAP;
is_trap(?TRAP, ?SAFE, ?SAFE) ->
    ?TRAP;
is_trap(?SAFE, ?SAFE, ?TRAP) ->
    ?TRAP;
is_trap(_, _, _) ->
    ?SAFE.

safe_tiles(Row) ->
    length(lists:filter(fun(C) -> C == ?SAFE end, Row)).

count_tiles(Start, Rows) ->
    {Tiles, _} =
        lists:foldl(fun(_, {N, AccIn}) ->
                       AccOut = next_row(AccIn),
                       {N + safe_tiles(AccOut), AccOut}
                    end,
                    {safe_tiles(Start), Start},
                    lists:seq(1, Rows - 1)),
    Tiles.
