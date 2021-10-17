-module(aoc2015_day05).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 5,
                name = "Doesn't He Have Intern-Elves For This?",
                expected = {238, 69},
                has_input_file = true}.

-type input_type() :: [nonempty_string()].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    string:tokens(binary_to_list(Input), "\n").

-spec solve1(Words :: input_type()) -> result1_type().
solve1(Words) ->
    length(lists:filter(fun is_nice_string/1, Words)).

-spec solve2(Words :: input_type()) -> result2_type().
solve2(Words) ->
    length(lists:filter(fun is_really_nice_string/1, Words)).

is_nice_string(S) ->
  at_least_three_vowels(S) andalso
    twice_in_a_row(S) andalso
    no_ugly_pairs(S).

at_least_three_vowels(S) ->
  length(lists:filter(fun is_vowel/1, S)) >= 3.

twice_in_a_row([]) -> false;
twice_in_a_row([X, X|_]) -> true;
twice_in_a_row([_|L]) -> twice_in_a_row(L).

no_ugly_pairs(L) ->
  lists:all(fun(S) ->
                string:find(L, S) =:= nomatch
            end, ["ab", "cd", "pq", "xy"]).

is_vowel($a) -> true;
is_vowel($e) -> true;
is_vowel($i) -> true;
is_vowel($o) -> true;
is_vowel($u) -> true;
is_vowel(_) -> false.

is_really_nice_string(S) ->
  has_pair_with_letter_between(S) andalso
    has_non_overlapping_pair(S).

has_pair_with_letter_between([]) -> false;
has_pair_with_letter_between([X,_,X|_]) -> true;
has_pair_with_letter_between([_|Rest]) ->
  has_pair_with_letter_between(Rest).

has_non_overlapping_pair(S) ->
  has_non_overlapping_pair(S, 0, #{}).

has_non_overlapping_pair([], _, _) -> false;
has_non_overlapping_pair([_], _, _) -> false;
has_non_overlapping_pair([X,Y|Rest], N, Map) ->
  %% The map keeps track of the position of the first occurrence of
  %% each pair we have seen, so when we see the same pair again at
  %% least 2 chars apart, we know we have two non-overlapping pairs.
  case maps:get({X, Y}, Map, undefined) of
    Pos when is_integer(Pos) and ((N - Pos) >= 2) ->
      true;
    undefined ->
      Map0 = maps:put({X, Y}, N, Map),
      has_non_overlapping_pair([Y|Rest], N + 1, Map0);
    _ ->
      has_non_overlapping_pair([Y|Rest], N + 1, Map)
  end.
