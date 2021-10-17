%% Behavior module for AoC puzzles

-module(aoc_puzzle).

-include("aoc_puzzle.hrl").

-include_lib("stdlib/include/assert.hrl").

-export([info/1, parse/2, solve/2, solve1/2, solve2/2, run_puzzle/1, read_input/1,
         mktest/1, label/1]).

-optional_callbacks([solve/1, solve1/1, solve2/1]).

%% Callback to parse the input data
-callback parse(Input :: binary()) -> ParsedInput :: term().
%% Callback to part 1 of the puzzle
-callback solve1(ParsedInput :: term()) -> term().
%% Callback to part 2 of the puzzle
-callback solve2(ParsedInput :: term()) -> term().
%% Callback for puzzles where we solve both puzzles in one call
-callback solve(ParsedInput :: term()) -> term().
%% Callback to get info about a puzzle
-callback info() -> aoc_puzzle().

info(M) ->
    M:info().

parse(M, Binary) ->
    M:parse(Binary).

solve(M, Input) ->
    M:solve(Input).

solve1(M, Input) ->
    M:solve1(Input).

solve2(M, Input) ->
    M:solve2(Input).

read_input(Info) ->
    Year = Info#aoc_puzzle.year,
    Day = Info#aoc_puzzle.day,

    Filename =
        lists:flatten(
            io_lib:format("~s/inputs/~w/input~2..0w.txt", ["priv", Year, Day])),
    case Info#aoc_puzzle.has_input_file of
        true ->
            {ok, Binary} = file:read_file(Filename),
            Binary;
        false ->
            <<>>
    end.

run_puzzle(Info) ->
    Input = read_input(Info),
    run_puzzle(Info, Input).

run_puzzle(Info, Input) ->
    M = Info#aoc_puzzle.module,
    ParsedInput = M:parse(Input),
    case Info#aoc_puzzle.use_one_solver_fun of
        true ->
            P = aoc_puzzle:solve(M, ParsedInput),
            ?assertEqual(Info#aoc_puzzle.expected, P),
            ok;
        false ->
            P1 = aoc_puzzle:solve1(M, ParsedInput),
            P2 = aoc_puzzle:solve2(M, ParsedInput),
            ?assertEqual(Info#aoc_puzzle.expected, {P1, P2}),
            ok
    end.

-define(TIMEOUT, 360).

-spec mktest(#aoc_puzzle{} | integer()) ->
                {string(), {timeout, integer(), fun()}} | {string(), fun()}.
mktest(Day) when is_integer(Day) ->
    {lists:flatten(
         io_lib:format("Day ~p: -- not implemented --", [Day])),
     fun() -> ok end};
mktest(Info) ->
    {label(Info), {timeout, ?TIMEOUT, fun() -> aoc_puzzle:run_puzzle(Info) end}}.

-spec label(aoc_puzzle()) -> string().
label(Info) ->
    Year = Info#aoc_puzzle.year,
    Day = Info#aoc_puzzle.day,
    lists:flatten(io_lib:format("~w Day ~2..0w: ~s", [Year, Day, Info#aoc_puzzle.name])).
