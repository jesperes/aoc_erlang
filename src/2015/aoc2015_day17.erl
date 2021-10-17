-module(aoc2015_day17).

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 17,
                name = "No Such Thing as Too Much",
                expected = {1638, 17},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: [integer()].
-type result_type() :: {integer(), integer()}.

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    lists:map(fun list_to_integer/1, string:tokens(binary_to_list(Input), " \n\r")).

-spec solve(Input :: input_type()) -> result_type().
solve(Buckets) ->
    Volume = 150,
    BucketCombos =
        lists:filter(fun(BucketList) -> lists:sum(BucketList) == Volume end,
                     combinations(Buckets)),

    SortedOnLength = lists:sort(fun(A, B) -> length(A) =< length(B) end, BucketCombos),

    MinLen = length(lists:nth(1, SortedOnLength)),

    NumShortestCombos =
        length(lists:filter(fun(X) -> length(X) == MinLen end, SortedOnLength)),
    {length(BucketCombos), NumShortestCombos}.

combinations([]) ->
    [];
combinations([H | T]) ->
    CT = combinations(T),
    [[H]] ++ [[H | L] || L <- CT] ++ CT.
