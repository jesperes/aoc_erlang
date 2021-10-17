%%% Advent of Code solution for 2020 day 02.
%%% Created: 2020-12-02T05:50:52+00:00

-module(aoc2020_day02).
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
             , day = 2
             , name = "Password Philosophy"
             , expected = {660, 530}
             , has_input_file = true
             }.

-type input_type() :: [{Min :: integer(),
                        Max :: integer(),
                        C :: integer(),
                        Pwd :: string()}].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  Lines = string:tokens(binary_to_list(Input), "\r\n"),
  lists:map(
    fun(S) ->
        [A, B, [Letter], Pwd] = string:tokens(S, ":- "),
        {list_to_integer(A),
         list_to_integer(B),
         Letter,
         Pwd}
    end, Lines).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
  length(lists:filter(fun is_valid_password_1/1, Input)).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
  length(lists:filter(fun is_valid_password_2/1, Input)).

is_valid_password_1({A, B, Letter, Pwd}) ->
  N = number_of(Letter, Pwd),
  N >= A andalso N =< B.

number_of(_, "") -> 0;
number_of(C, [C|Rest]) -> 1 + number_of(C, Rest);
number_of(C, [_|Rest]) -> number_of(C, Rest).

is_valid_password_2({A, B, Letter, Pwd}) ->
  case {lists:nth(A, Pwd) =:= Letter, lists:nth(B, Pwd) =:= Letter} of
    {true, false} -> true;
    {false, true} -> true;
    _ -> false
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
