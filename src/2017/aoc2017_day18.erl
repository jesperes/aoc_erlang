-module(aoc2017_day18).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 18,
                name = "Duet",
                expected = {7071, 8001},
                has_input_file = true}.

-type op() :: snd | set | add | mul | mod | rcv | jgz.
-type instr() :: {op(), integer()} | {op(), integer(), integer()}.
-type program() :: #{integer() => instr()}.
-type input_type() :: program().
-type result_type() :: integer().

-record(state, {program = #{}, id = 0, pc = 0, regs = #{}, queue = []}).

-type state() :: #state{}.

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    Lines = string:tokens(binary_to_list(Binary), "\n\r"),
    maps:from_list(
        lists:zip(
            lists:seq(0, length(Lines) - 1),
            lists:map(fun(Line) ->
                         [Op | Rest] = string:tokens(Line, " "),
                         list_to_tuple([list_to_atom(Op)]
                                       ++ lists:map(fun ([X | _] = Arg)
                                                            when X >= $a andalso X =< $z ->
                                                            list_to_atom(Arg);
                                                        (X) -> list_to_integer(X)
                                                    end,
                                                    Rest))
                      end,
                      Lines))).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Program) ->
    run_program1(#state{program = Program, queue = empty}).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Program) ->
    S0 = #state{program = Program,
                id = 0,
                regs = #{p => 0}},
    S1 = #state{program = Program,
                id = 1,
                regs = #{p => 1}},
    run_program2(S0, S1, 0, wait).

% Part 1: run program until first rcv
run_program1(#state{queue = _Queue, pc = _PC} = State) ->
    % ?debugFmt("run_program1/1, pc = ~p, queue = ~p", [PC, Queue]),
    case execute(State) of
        {rcv, _, _} ->
            State#state.queue;
        {snd, Y, StateOut} ->
            run_program1(StateOut#state{pc = StateOut#state.pc + 1,
                                        queue = read(Y, StateOut#state.regs)})
    end.

% Part 2: run two programs sending messages to each other
-spec run_program2(A :: state(),
                   B :: state(),
                   Count :: integer(),
                   Prev :: wait | snd | rcv) ->
                      integer().
run_program2(A, B, Count, Prev) ->
    % ?debugFmt("Executing, Count = ~p", [Count]),
    {D0, A0, B0} = do_run_program2(A, B),
    case {Prev, D0} of
        {wait, wait} ->
            % deadlock, return the number of messages
            % sent by p0
            Count;
        {snd, Action} when A#state.id =:= 0 ->
            % if P0 sent a message, update the count
            run_program2(B0, A0, Count + 1, Action);
        {_, Action} ->
            run_program2(B0, A0, Count, Action)
    end.

% Execute program A until it either sends or receives a message.
-spec do_run_program2(A :: state(), B :: state()) ->
                         {snd, state(), state()} |
                         {rcv, state(), state()} |
                         {wait, state(), state()}.
do_run_program2(A, B) ->
    case execute(A) of
        {snd, X, StateOut} ->
            Value = read(X, StateOut#state.regs),
            NewQueue = B#state.queue ++ [Value],
            {snd, StateOut#state{pc = StateOut#state.pc + 1}, B#state{queue = NewQueue}};
        {rcv, X, StateOut} ->
            case StateOut#state.queue of
                [Y | NewQueue] ->
                    NewRegs = maps:put(X, Y, StateOut#state.regs),
                    {rcv,
                     StateOut#state{pc = StateOut#state.pc + 1,
                                    regs = NewRegs,
                                    queue = NewQueue},
                     B};
                [] ->
                    {wait, StateOut, B}
            end
    end.

%% Helper functions

read_instr(#state{pc = PC, program = Program}) ->
    maps:get(PC, Program, eop).

read(X, Regs) when is_atom(X) ->
    maps:get(X, Regs, 0);
read(X, _Regs) when is_integer(X) ->
    X.

%% Execute until either a rcv or a snd instruction is encountered.
-spec execute(State :: state()) -> {snd, integer(), state()} | {rcv, atom(), state()}.
execute(#state{pc = PC, regs = Regs} = State) ->
    case read_instr(State) of
        {set, X, Y} ->
            execute(State#state{pc = PC + 1, regs = maps:put(X, read(Y, Regs), Regs)});
        {add, X, Y} ->
            execute(State#state{pc = PC + 1,
                                regs = maps:put(X, read(X, Regs) + read(Y, Regs), Regs)});
        {mul, X, Y} ->
            execute(State#state{pc = PC + 1,
                                regs = maps:put(X, read(X, Regs) * read(Y, Regs), Regs)});
        {mod, X, Y} ->
            execute(State#state{pc = PC + 1,
                                regs = maps:put(X, read(X, Regs) rem read(Y, Regs), Regs)});
        {snd, X} ->
            {snd, X, State};
        {rcv, X} ->
            {rcv, X, State};
        {jgz, X, Y} ->
            case read(X, Regs) of
                Value when Value > 0 ->
                    execute(State#state{pc = PC + read(Y, Regs)});
                _ ->
                    execute(State#state{pc = PC + 1})
            end
    end.

%% Tests

-ifdef(TEST).

%% ...

% ex1_test() ->
%     Input =
%         <<"set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz "
%           "a -1\nset a 1\njgz a -2\n">>,

%     Program = parse(Input),
%     X = solve1(Program),
%     ?assertEqual(4, X).

ex2_test() ->
    Input = <<"snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d\n">>,
    Program = parse(Input),
    ?assertEqual(3, solve2(Program)).

-endif.
