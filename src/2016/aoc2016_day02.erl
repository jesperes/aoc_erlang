-module(aoc2016_day02).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 2,
                name = "Bathroom Security",
                expected = {"56983", "8B8B1"},
                has_input_file = true}.

-type input_type() :: [string()].
-type result1_type() :: string().
-type result2_type() :: string().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    string:tokens(binary_to_list(Input), "\n\r").

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    lists:map(fun do_line1/1, Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    lists:map(fun do_line2/1, Input).

do_line1(Line) ->
    do_line(Line, fun move/2, 5).

do_line2(Line) ->
    do_line(Line, fun move2/2, 5).

do_line([], _, Pos) when Pos =< $9 ->
    Pos + $0;
do_line([], _, Pos) ->
    Pos;
do_line([C | Rest], Fun, Pos) ->
    do_line(Rest, Fun, Fun(Pos, C)).

%% 1 2 3
%% 4 5 6
%% 7 8 9
move(1, $L) ->
    1;
move(1, $U) ->
    1;
move(1, $D) ->
    4;
move(1, $R) ->
    2;
move(2, $L) ->
    1;
move(2, $U) ->
    2;
move(2, $D) ->
    5;
move(2, $R) ->
    3;
move(3, $L) ->
    2;
move(3, $U) ->
    3;
move(3, $D) ->
    6;
move(3, $R) ->
    3;
move(4, $L) ->
    4;
move(4, $U) ->
    1;
move(4, $D) ->
    7;
move(4, $R) ->
    5;
move(5, $L) ->
    4;
move(5, $U) ->
    2;
move(5, $D) ->
    8;
move(5, $R) ->
    6;
move(6, $L) ->
    5;
move(6, $U) ->
    3;
move(6, $D) ->
    9;
move(6, $R) ->
    6;
move(7, $L) ->
    7;
move(7, $U) ->
    4;
move(7, $D) ->
    7;
move(7, $R) ->
    8;
move(8, $L) ->
    7;
move(8, $U) ->
    5;
move(8, $D) ->
    8;
move(8, $R) ->
    9;
move(9, $L) ->
    8;
move(9, $U) ->
    6;
move(9, $D) ->
    9;
move(9, $R) ->
    9.

%%     1
%%   2 3 4
%% 5 6 7 8 9
%%   A B C
%%     D
move2(1, $R) ->
    1;
move2(1, $L) ->
    1;
move2(1, $U) ->
    1;
move2(1, $D) ->
    3;
move2(2, $R) ->
    3;
move2(2, $L) ->
    2;
move2(2, $U) ->
    2;
move2(2, $D) ->
    6;
move2(3, $R) ->
    4;
move2(3, $L) ->
    2;
move2(3, $U) ->
    1;
move2(3, $D) ->
    7;
move2(4, $R) ->
    4;
move2(4, $L) ->
    3;
move2(4, $U) ->
    4;
move2(4, $D) ->
    8;
move2(5, $R) ->
    6;
move2(5, $L) ->
    5;
move2(5, $U) ->
    5;
move2(5, $D) ->
    5;
move2(6, $R) ->
    7;
move2(6, $L) ->
    5;
move2(6, $U) ->
    2;
move2(6, $D) ->
    $A;
move2(7, $R) ->
    8;
move2(7, $L) ->
    6;
move2(7, $U) ->
    3;
move2(7, $D) ->
    $B;
move2(8, $R) ->
    9;
move2(8, $L) ->
    7;
move2(8, $U) ->
    4;
move2(8, $D) ->
    $C;
move2(9, $R) ->
    9;
move2(9, $L) ->
    8;
move2(9, $U) ->
    9;
move2(9, $D) ->
    9;
move2($A, $R) ->
    $B;
move2($A, $L) ->
    $A;
move2($A, $U) ->
    6;
move2($A, $D) ->
    $A;
move2($B, $R) ->
    $C;
move2($B, $L) ->
    $A;
move2($B, $U) ->
    7;
move2($B, $D) ->
    $D;
move2($C, $R) ->
    $C;
move2($C, $L) ->
    $B;
move2($C, $U) ->
    8;
move2($C, $D) ->
    $C;
move2($D, $R) ->
    $D;
move2($D, $L) ->
    $D;
move2($D, $U) ->
    $B;
move2($D, $D) ->
    $D.
