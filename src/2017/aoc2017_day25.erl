-module(aoc2017_day25).

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 25,
                name = "The Halting Problem",
                expected = 2725,
                use_one_solver_fun = true,
                has_input_file = false}.

-type state_name() :: 'A' | 'B' | 'C' | 'D' | 'E' | 'F'.
-type tape_value() :: 0 | 1.
-type tape_dir() :: -1 | 1.
-type state() ::
    {WriteValue :: tape_value(), Direction :: tape_dir(), NextState :: state_name()}.
-type tape() :: #{integer() := tape_value()}.

-record(tm,
        {current_state :: state_name(),
         steps :: integer(),
         tape = #{} :: tape(),
         cursor = 0 :: integer(),
         states :: #{state_name() := #{tape_value() := state()}}}).

-type tm() :: #tm{}.
-type input_type() :: tm().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(_Binary) ->
    #tm{current_state = 'A',
        steps = 12368930,
        states =
            #{'A' => #{0 => {1, 1, 'B'}, 1 => {0, 1, 'C'}},
              'B' => #{0 => {0, -1, 'A'}, 1 => {0, 1, 'D'}},
              'C' => #{0 => {1, 1, 'D'}, 1 => {1, 1, 'A'}},
              'D' => #{0 => {1, -1, 'E'}, 1 => {0, -1, 'D'}},
              'E' => #{0 => {1, 1, 'F'}, 1 => {1, -1, 'B'}},
              'F' => #{0 => {1, 1, 'A'}, 1 => {1, 1, 'E'}}}}.

-spec solve(Input :: input_type()) -> result_type().
solve(TM) ->
    execute(TM, 0).

-spec execute(TM :: tm(), Limit :: integer()) -> integer().
execute(#tm{steps = Steps, tape = Tape}, N) when Steps == N ->
    maps:size(Tape);
execute(TM, N) ->
    CurrentState = maps:get(TM#tm.current_state, TM#tm.states),
    CurrentValue = maps:get(TM#tm.cursor, TM#tm.tape, 0),
    {WriteValue, Dir, NextState} = maps:get(CurrentValue, CurrentState),
    NewTape = write(WriteValue, TM#tm.cursor, TM#tm.tape),
    NewCursor = TM#tm.cursor + Dir,
    execute(TM#tm{current_state = NextState,
                  tape = NewTape,
                  cursor = NewCursor},
            N + 1).

-spec write(tape_value(), integer(), tape()) -> tape().
write(1, Cursor, Tape) ->
    maps:put(Cursor, 1, Tape);
write(0, Cursor, Tape) ->
    maps:remove(Cursor, Tape).
