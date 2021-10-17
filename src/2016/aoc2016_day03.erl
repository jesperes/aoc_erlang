-module(aoc2016_day03).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 3,
                name = "Squares With Three Sides",
                expected = {917, 1649},
                has_input_file = true}.

-type input_type() :: [{integer(), integer(), integer()}].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    Lines = string:tokens(binary_to_list(Input), "\n\r"),
    lists:map(fun(Line) ->
                 [A, B, C] = string:tokens(Line, " "),
                 {list_to_integer(A), list_to_integer(B), list_to_integer(C)}
              end,
              Lines).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    count_triangles(Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    count_triangles(flip(Input)).

count_triangles(L) ->
    length(lists:filter(fun is_triangle/1, L)).

is_triangle({A, B, C}) ->
    A + B > C andalso B + C > A andalso A + C > B.

flip(L) ->
    flip(L, []).

flip([], Acc) ->
    Acc;
flip([{A1, B1, C1}, {A2, B2, C2}, {A3, B3, C3} | Rest], Acc) ->
    flip(Rest, [{A1, A2, A3}, {B1, B2, B3}, {C1, C2, C3} | Acc]).
