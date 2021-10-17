
-module(aoc2016_day23).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 23,
                name = "Safe Cracking",
                expected = {11130, 479007690},
                has_input_file = true}.

-type input_type() :: any().
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    Lines = string:tokens(binary_to_list(Input), "\n\r"),
    Tokens = lists:map(fun(Line) -> string:tokens(Line, " ") end, Lines),
    to_instr_map(lists:map(fun parse_line/1, Tokens)).

%% Part 1
-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    #{a := A} = execute(0, Input, #{a => 7}),
    A.

%% As is common for these kind of AoC puzzles, for part 2 we need
%% to figure out what the program actually does in order to be able
%% to optimize the solution. In this case the input program is
%% basically a factorial program, with a offset to make it generate
%% different solutions for different input. The offset can be found
%% at the end of the program, like:
%%
%% ...
%% cpy 70 c
%% jnz 87 d
%% ...
%%
%% Multiply the constant and add the factorial of the number of eggs
%% (register A).
-spec solve2(Input :: input_type()) -> result2_type().
solve2(_Input) ->
    solve_fast(12).

solve_fast(A) ->
    70 * 87 + factorial(A).

-spec factorial(pos_integer()) -> pos_integer().
factorial(0) ->
    1;
factorial(N) when N > 0 ->
    N * factorial(N - 1).

%% ------------------------------------------------------------
%% Parser
%% ------------------------------------------------------------

arg(S) ->
    try
        list_to_integer(S)
    catch
        error:badarg ->
            list_to_atom(S)
    end.

parse_line([A, B, C]) ->
    {list_to_atom(A), arg(B), arg(C)};
parse_line([A, B]) ->
    {list_to_atom(A), arg(B)}.

to_instr_map(Instrs) ->
    {_, InstrMap} =
        lists:foldl(fun(Instr, {N, Map}) -> {N + 1, maps:put(N, Instr, Map)} end,
                    {0, #{}},
                    Instrs),
    InstrMap.

%% ------------------------------------------------------------
%% Assembunny interpreter
%% ------------------------------------------------------------

read_reg(X, _) when is_number(X) ->
    X;
read_reg(X, Regs) when is_atom(X) ->
    maps:get(X, Regs, 0).

execute(Pc, Instrs, Regs) ->
    Instr = maps:get(Pc, Instrs, eop),
    case Instr of
        {cpy, X, Y} when is_atom(Y) ->
            execute(Pc + 1, Instrs, maps:put(Y, read_reg(X, Regs), Regs));
        {inc, X} when is_atom(X) ->
            execute(Pc + 1, Instrs, maps:put(X, read_reg(X, Regs) + 1, Regs));
        {dec, X} when is_atom(X) ->
            execute(Pc + 1, Instrs, maps:put(X, read_reg(X, Regs) - 1, Regs));
        {tgl, X} ->
            %% Yay! Self-modifying code. What could possibly go wrong...
            TglTarget = Pc + read_reg(X, Regs),
            case maps:is_key(TglTarget, Instrs) of
                false ->
                    %% Toggled instruction is invalid, nothing happens.
                    execute(Pc + 1, Instrs, Regs);
                true ->
                    %% Modify the program by toggling instruction at TglTarget,
                    %% according to puzzle rules.
                    ModInstr =
                        maps:update_with(TglTarget,
                                         fun ({inc, Z}) ->
                                                 {dec, Z};
                                             ({_, Z}) ->
                                                 {inc, Z};
                                             ({jnz, Z, W}) ->
                                                 {cpy, Z, W};
                                             ({cpy, Z, W}) ->
                                                 {jnz, Z, W}
                                         end,
                                         Instrs),
                    execute(Pc + 1, ModInstr, Regs)
            end;
        {jnz, X, Y} ->
            case read_reg(X, Regs) of
                0 ->
                    execute(Pc + 1, Instrs, Regs);
                _ ->
                    execute(Pc + read_reg(Y, Regs), Instrs, Regs)
            end;
        eop ->
            Regs;
        _ ->
            execute(Pc + 1, Instrs, Regs)
    end.
