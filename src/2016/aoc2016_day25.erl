-module(aoc2016_day25).

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 25,
                name = "Clock Signal",
                expected = 180,
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: map().
-type result_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    Lines = string:tokens(binary_to_list(Input), "\n\r"),
    {_, Prog} = lists:foldl(fun parse_line/2, {0, #{}}, Lines),
    Prog.

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    {found, Found} = find_clock_input(Input, 0, 100000),
    Found.

read_value(Reg, Regs) ->
    case Reg of
        [C] when (C >= $a) and (C =< $z) ->
            maps:get(Reg, Regs, 0);
        _ ->
            list_to_integer(Reg)
    end.

write_reg(Reg, Value, Regs) ->
    Regs#{Reg => Value}.

interpret(_, _, Regs, 0) ->
    Regs;
interpret(PC, Prog, Regs, MaxCycles) ->
    Instr = maps:get(PC, Prog, eop),
    Next =
        case Instr of
            eop ->
                Regs;
            {cpy, X, Y} ->
                {PC + 1, write_reg(Y, read_value(X, Regs), Regs)};
            {inc, X} ->
                {PC + 1, write_reg(X, read_value(X, Regs) + 1, Regs)};
            {dec, X} ->
                {PC + 1, write_reg(X, read_value(X, Regs) - 1, Regs)};
            {out, X} ->
                {PC + 1,
                 maps:update_with(output, fun(V) -> [read_value(X, Regs) | V] end, [], Regs)};
            {jnz, X, Y} ->
                case read_value(X, Regs) of
                    0 ->
                        {PC + 1, Regs};
                    _ ->
                        {PC + read_value(Y, Regs), Regs}
                end
        end,

    case Next of
        R when R =:= Regs, is_map(Regs) ->
            Regs;
        {PC0, Regs1} ->
            interpret(PC0, Prog, Regs1, MaxCycles - 1)
    end.

find_clock_input(Prog, A, MaxCycles) ->
    Regs = interpret(0, Prog, #{"a" => A}, MaxCycles),
    case is_alternating(maps:get(output, Regs)) of
        true ->
            {found, A};
        _ ->
            find_clock_input(Prog, A + 1, MaxCycles)
    end.

is_alternating([_]) ->
    true;
is_alternating([X, Y | Rest]) when X /= Y ->
    is_alternating([Y | Rest]);
is_alternating([X, X | _]) ->
    false.

%%% Parser

parse_line(Line, {PC, Map}) ->
    Tokens = string:tokens(Line, " "),
    Instr =
        case Tokens of
            ["cpy", X, Y] ->
                {cpy, X, Y};
            ["inc", X] ->
                {inc, X};
            ["dec", X] ->
                {dec, X};
            ["jnz", X, Y] ->
                {jnz, X, Y};
            ["out", X] ->
                {out, X}
        end,
    {PC + 1, Map#{PC => Instr}}.
