-module(aoc2021_day04).

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-include("aoc_puzzle.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 4,
                name = "Giant Squid",
                expected = {33462, 30070},
                has_input_file = true,
                use_one_solver_fun = true}.

-type input_type() :: {[integer()], [[integer()]]}.
-type result_type() :: any().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    [First, Rest] = binary:split(Binary, <<"\n">>),
    Numbers = lists:map(fun binary_to_integer/1,
                        split(First, <<",">>)),
    Boards = parse_boards(Rest),
    {Numbers, Boards}.

-spec solve(Input :: input_type()) -> result_type().
solve({Numbers, Boards}) ->
    play_bingo(Numbers, [], Boards, []).

%% {P1, P2};
play_bingo([], _, _, Winners) ->
    First = hd(Winners),
    Last = hd(lists:reverse(Winners)),
    {solution(First), solution(Last)};
play_bingo([Number|Rest], Played, Boards, Winners) ->
    NewPlayed = [Number|Played],
    WinningBoards =
        lists:filter(
          fun(Board) ->
                  check_bingo_board(NewPlayed, Board)
          end, Boards),
    case WinningBoards of
        [] ->
            play_bingo(Rest, NewPlayed, Boards -- WinningBoards,
                       Winners);

        [WinningBoard|_] ->
            play_bingo(Rest, NewPlayed, Boards -- WinningBoards,
                       Winners ++ [{NewPlayed, WinningBoard}])
    end.


solution({[Num|_] = PlayedNumbers, Board}) ->
    Num * lists:foldl(
            fun(B, Sum) ->
                    case lists:member(B, PlayedNumbers) of
                        true -> Sum;
                        false -> B + Sum
                    end
            end, 0, Board).

check_bingo_board(Numbers, Board) ->
    is_horizontal(1, Numbers, Board) orelse
        is_horizontal(2, Numbers, Board) orelse
        is_horizontal(3, Numbers, Board) orelse
        is_horizontal(4, Numbers, Board) orelse
        is_horizontal(5, Numbers, Board) orelse
        is_vertical(1, Numbers, Board) orelse
        is_vertical(2, Numbers, Board) orelse
        is_vertical(3, Numbers, Board) orelse
        is_vertical(4, Numbers, Board) orelse
        is_vertical(5, Numbers, Board).

is_horizontal(N, Numbers, Board) ->
    RowN = lists:sublist(Board, (N - 1) * 5 + 1, 5),
    contains_all(RowN, Numbers).

is_vertical(N, Numbers, Board) ->
    ColN = column(N, Board),
    contains_all(ColN, Numbers).

column(_N, []) ->
    [];
column(N, Board) ->
    {Row, Rest} = lists:split(5, Board),
    [lists:nth(N, Row)|column(N, Rest)].

contains_all(Elements, List) ->
    lists:all(fun(E) ->
                      lists:member(E, List)
              end, Elements).


%% ============================================================
%% Input parser
%% ============================================================

parse_boards(Binary) ->
    Rows = lists:map(
             fun(Row) ->
                     lists:map(
                       fun binary_to_integer/1,
                       split(Row, <<" ">>))
             end, split(Binary, <<"\n">>)),
    parse_boards0(Rows).

parse_boards0([]) -> [];
parse_boards0([R1, R2, R3, R4, R5|Rest]) ->
    [R1 ++ R2 ++ R3 ++ R4 ++ R5|parse_boards0(Rest)].

split(Bin, Pattern) ->
    binary:split(Bin, Pattern, [global, trim_all]).

%% Tests
-ifdef(TEST).

column_test() ->
    ?assertEqual([3, 8, 13, 18, 23], column(3, lists:seq(1, 25))).

is_horizontal_test() ->
    ?assert(is_horizontal(3, [11, 12, 13, 14, 15], lists:seq(1, 25))).

is_vertical_test() ->
    ?assert(is_vertical(3, [3, 8, 13, 18, 23], lists:seq(1, 25))).

check_bingo_board_test() ->
    ?assert(check_bingo_board([11, 12, 13, 14, 15], lists:seq(1, 25))).

ex_input() ->
    <<"7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n"
      "\n"
      "22 13 17 11  0\n"
      " 8  2 23  4 24\n"
      "21  9 14 16  7\n"
      " 6 10  3 18  5\n"
      " 1 12 20 15 19\n"
      "\n"
      " 3 15  0  2 22\n"
      " 9 18 13 17  5\n"
      "19  8  7 25 23\n"
      "20 11 10 24  4\n"
      "14 21 16 12  6\n"
      "\n"
      "14 21 17 24  4\n"
      "10 16 15  9 19\n"
      "18  8 23 26 20\n"
      "22 11 13  6  5\n"
      " 2  0 12  3  7\n">>.

ex1_test() ->
    Input = parse(ex_input()),
    ?assertEqual({4512, 1924}, solve(Input)).

ex2_test() ->
    ?assert(is_horizontal(1, lists:sort([14, 21, 17, 24, 4]),
                          [14, 21, 17, 24, 4,
                           10, 16, 15, 9, 19,
                           18, 8, 23, 26, 20,
                           22, 11, 13, 6, 5,
                           2, 0, 12, 3, 7])),
    ?assert(check_bingo_board(lists:sort([14, 21, 17, 24, 4]),
                              [14, 21, 17, 24, 4,
                               10, 16, 15, 9, 19,
                               18, 8, 23, 26, 20,
                               22, 11, 13, 6, 5,
                               2, 0, 12, 3, 7])).

-endif.
