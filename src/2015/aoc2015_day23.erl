-module(aoc2015_day23).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 23,
                name = "Opening the Turing Lock",
                expected = {255, 334},
                has_input_file = true}.

-type input_type() :: map().
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    P = lists:foldl(fun parse_line/2,
                    #{pc => 0},
                    string:tokens(binary_to_list(Input), "\n\r")),
    maps:remove(pc, P). %% pc is only used temporarily during parsing

-spec solve1(Program :: input_type()) -> result1_type().
solve1(Program) ->
    execute_program(Program,
                    #{pc => 0,
                      a => 0,
                      b => 0}).

-spec solve2(Program :: input_type()) -> result2_type().
solve2(Program) ->
    execute_program(Program,
                    #{pc => 0,
                      a => 1,
                      b => 0}).

execute_program(Prog, #{pc := PC} = Regs) ->
    case maps:get(PC, Prog, eop) of
        eop ->
            maps:get(b, Regs);
        Instr ->
            Regs0 = execute_instr(Instr, Regs),
            execute_program(Prog, Regs0)
    end.

%%% Instruction execution

execute_instr({inc, Reg}, Regs) ->
    incr_pc(maps:update_with(Reg, fun(V) -> V + 1 end, Regs));
execute_instr({hlf, Reg}, Regs) ->
    incr_pc(maps:update_with(Reg, fun(V) -> V div 2 end, Regs));
execute_instr({tpl, Reg}, Regs) ->
    incr_pc(maps:update_with(Reg, fun(V) -> V * 3 end, Regs));
execute_instr({jmp, Offset}, Regs) ->
    maps:update_with(pc, fun(V) -> V + Offset end, Regs);
execute_instr({jie, Reg, Offset}, Regs) ->
    case is_even(Reg, Regs) of
        true ->
            maps:update_with(pc, fun(V) -> V + Offset end, Regs);
        false ->
            incr_pc(Regs)
    end;
execute_instr({jio, Reg, Offset}, Regs) ->
    case is_one(Reg, Regs) of
        true ->
            maps:update_with(pc, fun(V) -> V + Offset end, Regs);
        false ->
            incr_pc(Regs)
    end.

%%% Parsing

parse_line(Line, #{pc := PC} = Prog) ->
    Prog#{PC => parse_tokens(string:tokens(Line, ", ")), pc => PC + 1}.

parse_tokens(["hlf", Reg]) ->
    {hlf, ltoa(Reg)};
parse_tokens(["tpl", Reg]) ->
    {tpl, ltoa(Reg)};
parse_tokens(["inc", Reg]) ->
    {inc, ltoa(Reg)};
parse_tokens(["jmp", Offset]) ->
    {jmp, ltoi(Offset)};
parse_tokens(["jie", Reg, Offset]) ->
    {jie, ltoa(Reg), ltoi(Offset)};
parse_tokens(["jio", Reg, Offset]) ->
    {jio, ltoa(Reg), ltoi(Offset)}.

%%% Helpers

ltoa(X) ->
    list_to_atom(X).

ltoi(X) ->
    list_to_integer(X).

incr_pc(Regs) ->
    maps:update_with(pc, fun(V) -> V + 1 end, Regs).

is_one(Reg, Regs) ->
    maps:get(Reg, Regs) == 1.

is_even(Reg, Regs) ->
    maps:get(Reg, Regs) rem 2 == 0.
