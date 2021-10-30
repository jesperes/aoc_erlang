-module(aoc2017_day15).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 15,
                name = "Dueling Generators",
                expected = {592, 320},
                has_input_file = false}.

-type input_type() :: {AStart :: integer(), BStart :: integer()}.
-type result_type() :: integer().

-define(A_FACTOR, 16807).
-define(B_FACTOR, 48271).
-define(MODULO, 2147483647).
-define(LOWER_16(X), X band 16#FFFF).

-spec parse(Binary :: binary()) -> input_type().
parse(_Binary) ->
    {277, 349}.

-spec solve1(Input :: input_type()) -> result_type().
solve1({A, B}) ->
    count_matching_pairs(A, B, 0, 40000000).

-spec solve2(Input :: input_type()) -> result_type().
solve2({A, B}) ->
    count_matching_pairs2(A, B, 0, 5000000).

count_matching_pairs(_A, _B, N, 0) ->
    N;
count_matching_pairs(A, B, N, Iter) ->
    ANext = A * ?A_FACTOR rem ?MODULO,
    BNext = B * ?B_FACTOR rem ?MODULO,
    case ?LOWER_16(ANext) == ?LOWER_16(BNext) of
        true ->
            count_matching_pairs(ANext, BNext, N + 1, Iter - 1);
        false ->
            count_matching_pairs(ANext, BNext, N, Iter - 1)
    end.

next_number(A, Factor, Rem) ->
    ANext = A * Factor rem ?MODULO,
    case ANext band Rem of
        0 ->
            ANext;
        _ ->
            next_number(ANext, Factor, Rem)
    end.

count_matching_pairs2(_A, _B, N, 0) ->
    N;
count_matching_pairs2(A, B, N, Iter) ->
    ANext = next_number(A, ?A_FACTOR, 3),
    BNext = next_number(B, ?B_FACTOR, 7),
    case ?LOWER_16(ANext) == ?LOWER_16(BNext) of
        true ->
            count_matching_pairs2(ANext, BNext, N + 1, Iter - 1);
        false ->
            count_matching_pairs2(ANext, BNext, N, Iter - 1)
    end.

%% Tests
-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

ex1_test() ->
    ?assertEqual(1, count_matching_pairs(65, 8921, 0, 5)),
    ?assertEqual(0, count_matching_pairs2(65, 8921, 0, 1055)),
    ?assertEqual(1, count_matching_pairs2(65, 8921, 0, 1056)).

-endif.
