%% Day 19: An Elephant Named Joseph
%%
%% This is a variant of the Josephus problem, which can be solved
%% arithmetically, see https://www.youtube.com/watch?v=uCsD3ZGzMgE.
%%
%% Part 2 can also be solved arithmetically, but I stole the formula
%% from Reddit:
%% https://www.reddit.com/r/adventofcode/comments/5j4lp1/2016_day_19_solutions/

-module(aoc2016_day19).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 19,
                name = "An Elephant Named Joseph",
                expected = {1834471, 1420064},
                has_input_file = false}.

-type input_type() :: integer().
-type result1_type() :: any().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    3014387.

-spec solve1(Input :: input_type()) -> result1_type().
solve1(N) ->
    2 * (N - largest_2pow(1, N)) + 1.

-spec solve2(Input :: input_type()) -> result2_type().
solve2(N) ->
    B = largest_3pow(1, N),
    if N == B ->
           N;
       N - B =< B ->
           N - B;
       true ->
           2 * N - 3 * B
    end.

%% Return the largest 2^A such that 2^A < N. I'm sure there is a nicer
%% way of doing this.
largest_2pow(A, N) ->
    if 1 bsl A > N ->
           1 bsl (A - 1);
       true ->
           largest_2pow(A + 1, N)
    end.

%% Return the largest 3^A < N.
largest_3pow(A, N) ->
    P = math:pow(3, A),
    if P > N ->
           floor(math:pow(3, A - 1));
       true ->
           largest_3pow(A + 1, N)
    end.
