-module(aoc2017_day17).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 17,
                name = "Spinlock",
                expected = {596, 39051595},
                has_input_file = false}.

-type input_type() :: integer().
-type result_type() :: integer().

-define(INPUT, 377).
-define(LIMIT_PART1, 2017).
-define(LIMIT_PART2, 50000000).

-spec parse(Binary :: binary()) -> input_type().
parse(_Binary) ->
    377.

-spec solve1(Input :: input_type()) -> result_type().
solve1(_Steps) ->
    spinlock(1, 0, [0]).

-spec solve2(Input :: input_type()) -> result_type().
solve2(_Steps) ->
    spinlock2(1, 0, undef).

spinlock(Value, Pos, List) when Value > ?LIMIT_PART1 ->
    {_, [_, X | _]} = lists:split(Pos, List),
    X;
spinlock(Value, Pos, List) ->
    Len = length(List),
    NewPos = (Pos + ?INPUT) rem length(List) + 1,
    NewList =
        if NewPos == Len ->
               List ++ [Value];
           true ->
               {A, B} = lists:split(NewPos, List),
               A ++ [Value] ++ B
        end,
    spinlock(Value + 1, NewPos, NewList).

spinlock2(Value, _, Target) when Value > ?LIMIT_PART2 ->
    Target;
spinlock2(Value, Pos, Target) ->
    NewPos = ((Pos + ?INPUT) rem Value) + 1,
    NewTarget =
        if NewPos == 1 ->
               Value;
           true ->
               Target
        end,
    spinlock2(Value + 1, NewPos, NewTarget).
