-module(aoc2015_day10).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 10,
                name = "Elves Look, Elves Say",
                expected = {492982, 6989950},
                has_input_file = false}.

-type input_type() :: string().
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(_) ->
    "1321131112".

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    iterate(40, Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    iterate(50, Input).

iterate(N, Input) ->
    Fun = fun(_, S) -> look_and_say(S) end,
    length(lists:foldl(Fun, Input, lists:seq(1, N))).

look_and_say(List) ->
    lists:flatten(look_and_say0(List)).

look_and_say0([]) -> [];
look_and_say0([X|_] = L) ->
    {Len, Rest} = split_prefix(X, L, 0),
    [integer_to_list(Len), [X], look_and_say0(Rest)].

split_prefix(_, [], N) -> {N, []};
split_prefix(X, [Y|_] = L, N) when X =/= Y -> {N, L};
split_prefix(X, [X|Rest], N) ->
    split_prefix(X, Rest, N + 1).
