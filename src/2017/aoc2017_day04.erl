-module(aoc2017_day04).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 4,
                name = "High-Entropy Passphrases",
                expected = {466, 251},
                has_input_file = true}.

-type passphrase() :: [string()].
-type input_type() :: [passphrase()].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    lists:map(fun(Line) -> string:tokens(Line, " ") end,
              string:tokens(binary_to_list(Input), "\n\r")).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    count(fun has_no_dup/1, Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    count(fun(Words) -> has_no_dup(Words) andalso has_no_anagram(Words) end, Input).

has_no_dup(Words) ->
    lists:sort(Words) =:= lists:usort(Words).

has_no_anagram(Words) ->
    [] =:= [W1 || W1 <- Words, W2 <- Words, W1 =/= W2, lists:sort(W1) =:= lists:sort(W2)].

count(Fun, List) ->
    length(lists:filter(Fun, List)).
