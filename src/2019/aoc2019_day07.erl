-module(aoc2019_day07).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).
%% internal exports
-export([amp/4]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 7,
                name = "Amplification Circuit",
                expected = {70597, 30872528},
                has_input_file = true}.

-type input_type() :: intcode:intcode_program().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    intcode:parse(Binary).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Prog) ->
    find_best_phase_setting(Prog).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Prog) ->
    find_best_phase_setting2(Prog).

%% --- Part 1 ---

chained_execute(_Prog, Input1, []) ->
    Input1;
chained_execute(Prog, Input1, [Input2 | Rest]) ->
    {_, [Output]} = intcode:execute(Prog, [Input2, Input1]),
    chained_execute(Prog, Output, Rest).

find_best_phase_setting(Prog) ->
    ThrustLevels =
        [begin
             PS = [X1, X2, X3, X4, X5],
             chained_execute(Prog, 0, PS)
         end
         || X1 <- lists:seq(0, 4),
            X2 <- lists:seq(0, 4),
            X3 <- lists:seq(0, 4),
            X4 <- lists:seq(0, 4),
            X5 <- lists:seq(0, 4),
            X1 =/= X2,
            X1 =/= X3,
            X1 =/= X4,
            X1 =/= X5,
            X2 =/= X3,
            X2 =/= X4,
            X2 =/= X5,
            X3 =/= X4,
            X3 =/= X5,
            X4 =/= X5],

    lists:max(ThrustLevels).

%% --- Part 2 ---
%%
%% One of the few puzzles where I've actually used Erlangs concurrency in a
%% natural way.

amp(Parent, Amp, DestAmp, Prog) when is_atom(Amp) ->
    register(Amp, self()),

    %% Register ourselves, and wait for start signal (so all other
    %% amplifiers are also registered.
    Parent ! {self(), registered},
    receive
        start ->
            ok
    end,

    DestPid = whereis(DestAmp),
    true = is_pid(DestPid),

    {_, FinalState} =
        intcode:execute(Prog,
                        fun(S) ->
                           %% Wait for next input and provide it to the input
                           %% instruction.
                           receive Input -> {S, Input} end
                        end,
                        fun(Out, State) ->
                           %% Send output values to the destination pid
                           DestPid ! Out,

                           %% Store the last seen output in the state, so that we can
                           %% fish it out when the program exits.
                           maps:put(out, Out, State)
                        end,
                        #{}),

    Parent ! {self(), maps:get(out, FinalState)}.

create_amplifiers(Prog, PhaseSettings) ->
    Params = lists:zip([{a, b}, {b, c}, {c, d}, {d, e}, {e, a}], PhaseSettings),
    Parent = self(),
    [A | _] =
        Pids =
            lists:map(fun({{Amp, DestAmp}, PS}) ->
                         Pid = spawn(?MODULE, amp, [Parent, Amp, DestAmp, Prog]),
                         Pid ! PS,
                         Pid
                      end,
                      Params),

    %% Sync amplifiers so they all have started before continuing (so no
    %% messages get lost).
    lists:foreach(fun(Pid) -> receive {Pid, registered} -> ok end end, Pids),
    lists:foreach(fun(Pid) -> Pid ! start end, Pids),

    %% Send start signal to 'a'
    A ! 0,

    %% Wait for output values, saving the one from the last amplifier.
    LastAmpPid = lists:last(Pids),

    FinalOut =
        lists:foldl(fun(Pid, Acc) ->
                       receive
                           {LastAmpPid, FinalOut} -> FinalOut;
                           {Pid, _FinalOut} -> Acc
                       end
                    end,
                    undefined,
                    Pids),
    FinalOut.

feedback_loop(Prog, PhaseSettings) ->
    create_amplifiers(Prog, PhaseSettings).

find_best_phase_setting2(Prog) ->
    ThrustLevels =
        [begin
             PS = [X1, X2, X3, X4, X5],
             feedback_loop(Prog, PS)
         end
         || X1 <- lists:seq(5, 9),
            X2 <- lists:seq(5, 9),
            X3 <- lists:seq(5, 9),
            X4 <- lists:seq(5, 9),
            X5 <- lists:seq(5, 9),
            X1 =/= X2,
            X1 =/= X3,
            X1 =/= X4,
            X1 =/= X5,
            X2 =/= X3,
            X2 =/= X4,
            X2 =/= X5,
            X3 =/= X4,
            X3 =/= X5,
            X4 =/= X5],

    lists:max(ThrustLevels).
