-module(aoc2016_day12).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 12,
                name = "Leonardo's Monorail",
                expected = {318003, 9227657},
                has_input_file = true}.

-type input_type() :: #{integer() => any()}.
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    Instrs =
        lists:map(fun(Line) ->
                     case string:tokens(Line, " ") of
                         [A, B, C] -> {list_to_atom(A), arg(B), arg(C)};
                         [A, B] -> {list_to_atom(A), arg(B)}
                     end
                  end,
                  string:tokens(binary_to_list(Input), "\n")),
    {_, InstrMap} =
        lists:foldl(fun(Instr, {N, Map}) -> {N + 1, maps:put(N, Instr, Map)} end,
                    {0, #{}},
                    Instrs),
    InstrMap.

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    #{a := A} = execute(0, Input, #{}),
    A.

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    #{a := A} = execute(0, Input, #{c => 1}),
    A.

arg(S) ->
    try
        list_to_integer(S)
    catch
        error:badarg ->
            list_to_atom(S)
    end.

read_reg(X, _) when is_number(X) ->
    X;
read_reg(X, Regs) when is_atom(X) ->
    maps:get(X, Regs, 0).

execute(Pc, Instrs, Regs) ->
    Instr = maps:get(Pc, Instrs, eop),
    case Instr of
        {cpy, X, Y} ->
            execute(Pc + 1, Instrs, maps:put(Y, read_reg(X, Regs), Regs));
        {inc, X} ->
            execute(Pc + 1, Instrs, maps:put(X, read_reg(X, Regs) + 1, Regs));
        {dec, X} ->
            execute(Pc + 1, Instrs, maps:put(X, read_reg(X, Regs) - 1, Regs));
        {jnz, X, Y} ->
            case read_reg(X, Regs) of
                0 ->
                    execute(Pc + 1, Instrs, Regs);
                _ ->
                    execute(Pc + Y, Instrs, Regs)
            end;
        eop ->
            Regs
    end.
