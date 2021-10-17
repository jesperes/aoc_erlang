-module(aoc2019_day04).


-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 4,
                name = "Secure Container",
                expected = {454, 288},
                use_one_solver_fun = true,
                has_input_file = false}.

-type input_type() :: {integer(), integer()}.
-type result_type() :: {integer(), integer()}.

-spec parse(Binary :: binary()) -> input_type().
parse(_Binary) ->
    {402328, 864247}.

-spec solve(Input :: input_type()) -> result_type().
solve({Lower, Upper}) ->
    Passwords = lists:seq(Lower, Upper),
    Part1 = length(lists:filter(fun is_part1/1, Passwords)),
    Part2 = length(lists:filter(fun is_part2/1, Passwords)),
    {Part1, Part2}.

is_part1(P) when is_integer(P) ->
    is_part1(integer_to_list(P));
is_part1([A, A, B, C, D, E]) when (B >= A) and (C >= B) and (D >= C) and (E >= D) ->
    true;
is_part1([A, B, B, C, D, E]) when (B >= A) and (C >= B) and (D >= C) and (E >= D) ->
    true;
is_part1([A, B, C, C, D, E]) when (B >= A) and (C >= B) and (D >= C) and (E >= D) ->
    true;
is_part1([A, B, C, D, D, E]) when (B >= A) and (C >= B) and (D >= C) and (E >= D) ->
    true;
is_part1([A, B, C, D, E, E]) when (B >= A) and (C >= B) and (D >= C) and (E >= D) ->
    true;
is_part1(_) ->
    false.

is_part2(P) when is_integer(P) ->
    is_part2(integer_to_list(P));
is_part2([A, A, B, C, D, E])
    when (B >= A) and (C >= B) and (D >= C) and (E >= D) and (A =/= B) ->
    true;
is_part2([A, B, B, C, D, E])
    when (B >= A) and (C >= B) and (D >= C) and (E >= D) and (A =/= B) and (B =/= C) ->
    true;
is_part2([A, B, C, C, D, E])
    when (B >= A) and (C >= B) and (D >= C) and (E >= D) and (B =/= C) and (C =/= D) ->
    true;
is_part2([A, B, C, D, D, E])
    when (B >= A) and (C >= B) and (D >= C) and (E >= D) and (C =/= D) and (D =/= E) ->
    true;
is_part2([A, B, C, D, E, E])
    when (B >= A) and (C >= B) and (D >= C) and (E >= D) and (D =/= E) ->
    true;
is_part2(_) ->
    false.
