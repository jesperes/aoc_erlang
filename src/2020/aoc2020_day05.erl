%%% Advent of Code solution for 2020 day 05.
%%% Created: 2020-12-05T06:22:29+00:00

-module(aoc2020_day05).
-behavior(aoc_puzzle).

-export([ parse/1
        , solve1/1
        , solve2/1
        , info/0
        ]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
  #aoc_puzzle{ module = ?MODULE
             , year = 2020
             , day = 5
             , name = "Binary Boarding"
             , expected = {928, 610}
             , has_input_file = true
             }.

-type input_type() :: [integer()].
-type result1_type() :: integer().
-type result2_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  %% We do most of the puzzle solving in the parser here, since it is
  %% common for both part 1 and 2.
  lists:sort(
    fun desc/2,
    lists:map(
      fun seat_id/1,
      string:split(string:trim(binary_to_list(Input)), "\n", all))).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
  %% Highest seat ID is first
  hd(Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
  find_seat(Input).

%% Sort function for descending order.
desc(A, B) -> B =< A.

%% Seat ids are really just the boarding card ids thinly disguised as
%% binary numbers
seat_id(S) ->
  lists:foldl(fun($B, Acc) -> (Acc bsl 1) bor 1;
                 ($R, Acc) -> (Acc bsl 1) bor 1;
                 (_, Acc)  -> Acc bsl 1
              end, 0, S).

%% The input to this function is a list of all occupied seats.  "Our"
%% seat is the only one missing, but it is specified to have a seat id
%% at -1 and +1, so we want to find the first "hole" in the sequence
%% of occupied seats.
find_seat([A, B|_]) when A == B + 2 -> B + 1;
find_seat([_|Rest]) -> find_seat(Rest).
