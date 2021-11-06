-module(aoc2017_day19).

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 19,
                name = "A Series Of Tubes",
                expected = {"EPYDUXANIT", 17544},
                has_input_file = true,
                use_one_solver_fun = true}.

-record(grid, {binary = <<>> :: binary(), width = 0 :: integer()}).

-type grid() :: #grid{}.
-type input_type() :: grid().
-type result_type() :: {string(), integer()}.

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    {Width, _} = binary:match(Binary, <<"\n">>),
    #grid{binary = Binary, width = Width + 1}.

-spec solve(Input :: input_type()) -> result_type().
solve(Grid) ->
    {Start, _} = binary:match(Grid#grid.binary, <<"|">>),
    trace(Start, down, Grid, [], 0).

-define(IS_LETTER(C), (C >= $A andalso C =< $Z)).
-define(IS_STRAIGHT(C), (C =:= $| orelse C =:= $- orelse ?IS_LETTER(C))).
-define(MAYBE_APPEND(C, Letters), if ?IS_LETTER(C) -> [C|Letters]; true -> Letters end).

left_of(Pos) ->
    Pos - 1.

right_of(Pos) ->
    Pos + 1.

below(Pos, #grid{width = Width}) ->
    Pos + Width.

above(Pos, #grid{width = Width}) ->
    Pos - Width.

at(Pos, #grid{binary = Binary}) ->
    binary:at(Binary, Pos).

trace(Pos, Direction, Grid, Letters, Steps) ->
    case {Direction, at(Pos, Grid)} of
        {down, C} when ?IS_STRAIGHT(C) ->
            trace(below(Pos, Grid), Direction, Grid, ?MAYBE_APPEND(C, Letters), Steps + 1);
        {up, C} when ?IS_STRAIGHT(C) ->
            trace(above(Pos, Grid), Direction, Grid, ?MAYBE_APPEND(C, Letters), Steps + 1);
        {left, C} when ?IS_STRAIGHT(C) ->
            trace(left_of(Pos), Direction, Grid, ?MAYBE_APPEND(C, Letters), Steps + 1);
        {right, C} when ?IS_STRAIGHT(C) ->
            trace(right_of(Pos), Direction, Grid, ?MAYBE_APPEND(C, Letters), Steps + 1);
        {C, $+} when C =:= up orelse C =:= down ->
            case {at(left_of(Pos), Grid), at(right_of(Pos), Grid)} of
                {$-, $ } ->
                    trace(left_of(Pos), left, Grid, Letters, Steps + 1);
                {$ , $-} ->
                    trace(right_of(Pos), right, Grid, Letters, Steps + 1)
            end;
        {C, $+} when C =:= left orelse C =:= right ->
            case {at(above(Pos, Grid), Grid), at(below(Pos, Grid), Grid)} of
                {$|, $ } ->
                    trace(above(Pos, Grid), up, Grid, Letters, Steps + 1);
                {$ , $|} ->
                    trace(below(Pos, Grid), down, Grid, Letters, Steps + 1)
            end;
        {_, $ } ->
            {lists:reverse(Letters), Steps}
    end.
