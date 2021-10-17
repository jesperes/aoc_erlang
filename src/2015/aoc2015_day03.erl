-module(aoc2015_day03).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 3,
                name = "Perfectly Spherical Houses in a Vacuum",
                expected = {2572, 2631},
                has_input_file = true}.

-type pos() :: {integer(), integer()}.
-type input_type() :: [byte()].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    binary_to_list(Input).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(List) ->
    StartPos = {0, 0},
    {_, Presents} =
        lists:foldl(fun(C, {Pos, Map}) ->
                       NewPos = next_pos(C, Pos),
                       {NewPos, incr_map_cntr(NewPos, Map)}
                    end,
                    {StartPos, #{StartPos => 1}},
                    List),
    maps:size(Presents).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(List) ->
    StartPos = {0, 0},
    {{_, _}, Presents} =
        lists:foldl(fun(C, {{Pos, PosOther}, Map}) ->
                       NewPos = next_pos(C, Pos),
                       {{PosOther, NewPos}, incr_map_cntr(NewPos, Map)}
                    end,
                    {{StartPos, StartPos}, #{StartPos => 2}},
                    List),
    maps:size(Presents).

-spec incr_map_cntr(pos(), map()) -> map().
incr_map_cntr(Key, Map) ->
    maps:update_with(Key, fun(V) -> V + 1 end, 1, Map).

-spec next_pos(integer(), pos()) -> pos().
next_pos($<, {X, Y}) ->
    {X - 1, Y};
next_pos($>, {X, Y}) ->
    {X + 1, Y};
next_pos($v, {X, Y}) ->
    {X, Y + 1};
next_pos($^, {X, Y}) ->
    {X, Y - 1}.
