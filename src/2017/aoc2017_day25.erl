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
%% (Ab)use the atomics module to serve as the Turing Machine tape.
-type tape() :: atomics:atomics_ref().

%% Tune tape size to be large enough.
-define(TAPE_SIZE, 8000).
-define(TAPE_START, ?TAPE_SIZE div 2).

-record(tm,
        {current_state :: state_name(),
         steps :: integer(),
         tape :: tape(),
         cursor = ?TAPE_START :: integer(),
         states :: #{state_name() := #{tape_value() := state()}}}).

-type tm() :: #tm{}.
-type input_type() :: tm().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(_Binary) ->
    #tm{current_state = 'A',
        steps = 12368930,
        tape = atomics:new(?TAPE_SIZE, []),
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
    count_ones(Tape);
execute(TM, N) ->
    State = maps:get(TM#tm.current_state, TM#tm.states),
    Value = read(TM#tm.cursor, TM#tm.tape),
    {WriteValue, Dir, NextState} = maps:get(Value, State),
    NewTape = write(WriteValue, TM#tm.cursor, TM#tm.tape),
    NewCursor = TM#tm.cursor + Dir,
    execute(TM#tm{current_state = NextState,
                  tape = NewTape,
                  cursor = NewCursor},
            N + 1).

-spec write(tape_value(), integer(), tape()) -> tape().
write(Value, Cursor, Tape) ->
    ok = atomics:put(Tape, Cursor, Value),
    Tape.

-spec read(tape_value(), tape()) -> integer().
read(Cursor, Tape) ->
    atomics:get(Tape, Cursor).

-spec count_ones(tape()) -> number().
count_ones(Tape) ->
    lists:foldl(fun(Ix, Acc) -> atomics:get(Tape, Ix) + Acc end, 0, lists:seq(1, ?TAPE_SIZE)).
