-module(aoc2017_day23).

-include("aoc_puzzle.hrl").

-export([parse/1, solve1/1, solve2/1, info/0]).

-behavior(aoc_puzzle).

%% Assembly optimization puzzle. The assembly code in the input is really a
%% program which counts the number of non-primes in an interval. Instead of
%% "interpreting" the assembly code, we just count the (non-)primes.
%%
%% See
%% https://www.reddit.com/r/adventofcode/comments/7lms6p/2017_day_23_solutions/
%% for explanations for how this works.

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 23,
                name = "Coprocessor Conflagration",
                expected = {6724, 903},
                has_input_file = true}.

-type input_type() :: integer().
-type result_type() :: {integer(), integer()}.

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    {match, [B]} = re:run(Binary, "^set b (\\d+)", [{capture, all_but_first, list}]),
    list_to_integer(B).

-spec solve1(Input :: input_type()) -> result_type().
solve1(B) ->
    (B - 2) * (B - 2).

-spec solve2(Input :: input_type()) -> result_type().
solve2(B) ->
    Start = B * 100 + 100000,
    End = Start + 17000,
    count_primes(Start, End, 0).

count_primes(N, End, Count) when N > End ->
    Count;
count_primes(N, End, Count) ->
    case is_prime(N) of
        true ->
            count_primes(N + 17, End, Count);
        false ->
            count_primes(N + 17, End, Count + 1)
    end.

is_prime(N) ->
    is_prime(N, 2, trunc(math:sqrt(N)) + 1).

is_prime(_, Max, Max) ->
    true;
is_prime(N, I, Max) ->
    if N rem I == 0 ->
           false;
       true ->
           is_prime(N, I + 1, Max)
    end.
