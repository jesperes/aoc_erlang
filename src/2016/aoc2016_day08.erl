-module(aoc2016_day08).

-include("aoc_puzzle.hrl").

-include_lib("stdlib/include/assert.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 8,
                name = "Two-Factor Authentication",
                expected =
                    {115,
                     ["####.####.####.#...##..#.####.###..####..###...##.",
                      "#....#....#....#...##.#..#....#..#.#......#.....#.",
                      "###..###..###...#.#.##...###..#..#.###....#.....#.",
                      "#....#....#......#..#.#..#....###..#......#.....#.",
                      "#....#....#......#..#.#..#....#.#..#......#..#..#.",
                      "####.#....####...#..#..#.#....#..#.#.....###..##.."]},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: any().
-type result_type() :: {any(), any()}.

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    string:tokens(binary_to_list(Input), "\n\r").

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    decode_and_execute(Input, {50, 6}).

rotate(X, N) ->
    {L1, L2} = lists:split(length(X) - N rem length(X), X),
    L2 ++ L1.

%%% Replace a member in a list
replace_list_member([], _, _) ->
    [];
replace_list_member([_ | List], 0, Member) ->
    [Member | List];
replace_list_member([X | List], N, Member) ->
    [X | replace_list_member(List, N - 1, Member)].

%%% Generate a display of the given dimensions.
generate_display(Width, Height) ->
    [[$. || _ <- lists:seq(1, Width)] || _ <- lists:seq(1, Height)].

%%% Replace the N first characters in a string.
replace_prefix(0, _, Xs) ->
    Xs;
replace_prefix(_, _, []) ->
    [];
replace_prefix(N, Char, [_X | Xs]) ->
    [Char | replace_prefix(N - 1, Char, Xs)].

%%% Return the Nth column as a list
nth_column([], _) ->
    [];
nth_column([Row | Rows], N) ->
    [lists:nth(N + 1, Row) | nth_column(Rows, N)].

%%% Replace the Nth column with the given list.
replace_nth_column([], [], _) ->
    [];
replace_nth_column([Row | Rows], [X | Xs], N) ->
    [replace_list_member(Row, N, X) | replace_nth_column(Rows, Xs, N)].

%%% Execute a single instruction. Returns the modified display.
execute_single_instr({rect, Width, Height}, Display) ->
    %% Fill WidthxHeight rectangle from top-left corner.
    NewDisplay =
        lists:map(fun(Row) -> replace_prefix(Width, $#, Row) end, lists:sublist(Display, Height))
        ++ lists:sublist(Display, Height + 1, length(Display) - Height),
    ?assert(length(NewDisplay) == length(Display)),
    NewDisplay;
execute_single_instr({rotate, row, {y, Row}, Steps}, Display) ->
    %% Rotate Row by the given number of Steps.
    RowToRotate = lists:nth(Row + 1, Display),
    RotatedRow = rotate(RowToRotate, Steps),
    NewDisplay = replace_list_member(Display, Row, RotatedRow),
    ?assert(length(NewDisplay) == length(Display)),
    NewDisplay;
execute_single_instr({rotate, column, {x, Col}, Steps}, Display) ->
    %% Rotate Col by the given number of Steps.
    ColumnToRotate = nth_column(Display, Col),
    RotatedColumn = rotate(ColumnToRotate, Steps),
    NewDisplay = replace_nth_column(Display, RotatedColumn, Col),
    ?assert(length(NewDisplay) == length(Display)),
    NewDisplay.

%%% Execute a list of instructions on the given display. Returns the
%%% modified display.
execute_instr([], Display) ->
    Display;
execute_instr([Instr | Rest], Display) ->
    execute_instr(Rest, execute_single_instr(Instr, Display)).

execute_instr(InstrList, Width, Height) ->
    execute_instr(InstrList, generate_display(Width, Height)).

count_pixels([]) ->
    0;
count_pixels([Row | Rows]) ->
    length(lists:filter(fun(X) -> X == $# end, Row)) + count_pixels(Rows).

-spec decode_and_execute(Instrs :: [string()], Size :: {integer(), integer()}) ->
                            {NumPixels :: integer(), Display :: [string()]}.
decode_and_execute(Instrs, {W, H} = _Size) ->
    InstrList = lists:map(fun decode_instr/1, Instrs),
    Display = execute_instr(InstrList, W, H),
    {count_pixels(Display), Display}.

decode_instr(Instr) ->
    [Op | Rest] = string:lexemes(Instr, " "),
    case list_to_atom(Op) of
        rect ->
            [Dim] = Rest,
            [Width, Height] = string:lexemes(Dim, "x"),
            {rect, list_to_integer(Width), list_to_integer(Height)};
        rotate ->
            [Direction, Op1, _, Op2] = Rest,
            [Var, Value] = string:lexemes(Op1, "="),
            {rotate,
             list_to_atom(Direction),
             {list_to_atom(Var), list_to_integer(Value)},
             list_to_integer(Op2)};
        _ ->
            true
    end.
