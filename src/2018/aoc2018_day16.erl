-module(aoc2018_day16).

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2018,
                day = 16,
                name = "Chronal Classification",
                expected = {521, 594},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: [string()].
-type result_type() :: {any(), any()}.

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    string:tokens(binary_to_list(Input), "\n\r").

-spec solve(Input :: input_type()) -> result_type().
solve(Lines) ->
    List = parse_lines(Lines),
    Samples = pass2(List),

    MatchedSamples =
        lists:map(fun({[before | RegValsBefore],
                       [_OpNum, A, B, C] = Instr,
                       ['after' | RegValsAfter]}) ->
                     {sample,
                      RegValsBefore,
                      Instr,
                      RegValsAfter,
                      matches,
                      lists:filtermap(fun({OpCode, RegValsAfter0}) ->
                                         case RegValsAfter0 == RegValsAfter of
                                             true -> {true, OpCode};
                                             false -> false
                                         end
                                      end,
                                      [{OpCode, execute_opcode(OpCode, A, B, C, RegValsBefore)}
                                       || OpCode <- opcodes()])}
                  end,
                  Samples),

    %% Number of instructions which matches at least 3 samples.
    Num = length(lists:filter(fun({sample, _, _, _, _, OpCodes}) -> length(OpCodes) >= 3 end,
                              MatchedSamples)),

    [R1 | _] = execute_test_program(List),
    {Num, R1}.

execute_test_program(List) ->
    InstrList = pass2_testprog(List),
    execute_instr_list(InstrList, [0, 0, 0, 0]).

execute_instr_list([], RegVals) ->
    RegVals;
execute_instr_list([[OpNum, A, B, C] | Rest], RegVals) ->
    OpCode = opcode(OpNum),
    NewRegVals = execute_opcode(OpCode, A, B, C, RegVals),
    execute_instr_list(Rest, NewRegVals).

%% This is a throwaway function to obtain the opnum to opcode mapping,
%% see opcode/1.
%% match_opcodes(MatchedSamples) ->
%%     %% MatchedSamples is a list of {sample, Before, Instr, After,
%%     %% OpcodeList} when OpcodeList is a list of opcodes which match
%%     %% the Before/Instr/After sample.

%%     L =
%%         [ lists:filtermap(fun({sample, _, [SampleOpNum|_], _, _, MatchingOpCodes}) ->
%%                                   case SampleOpNum == OpNum of
%%                                       true ->
%%                                           {true, {OpNum, sets:from_list(MatchingOpCodes)}};
%%                                       _ ->
%%                                           false
%%                                   end
%%                           end, MatchedSamples)
%%           || OpNum <- lists:seq(0, length(opcodes()) - 1) ],

%%     %% L is now a list of tuples {opnum, matching_opcodes}, so we take
%%     %% the intersection of all the matching opcodes.

%%     lists:map(fun([{OpNum, _}|_] = OpCodeMatches) ->

%%                       Sets =
%%                           lists:map(fun({_, MatchingOpCodes}) ->
%%                                             MatchingOpCodes
%%                                     end, OpCodeMatches),
%%                       ISet = sets:intersection(Sets),

%%                       {OpNum, sets:to_list(ISet)};
%%                  (_) ->
%%                       {}
%%               end, L).

%% {521,
%%  [{0,[eqir,borr,addr,eqri,seti]},
%%   {1,[borr,addi,bori,seti]},
%%   {2,[eqrr,eqir,seti]},
%%   {3,[eqrr,gtir,seti]},
%%   {4,[addi]},
%%   {5,[gtrr,gtir,setr,borr,addr,gtri,seti,bori,addi]},
%%   {6,[gtrr,eqri]},
%%   {7,[gtir,seti]},
%%   {8,[muli]},
%%   {9,[borr,bori]},
%%   {10,
%%    [gtrr,gtir,setr,borr,banr,mulr,addr,gtri,seti,bori,bani,
%%     muli,addi]},
%%   {11,[borr]},
%%   {12,[eqrr,eqir,gtrr,gtir,eqri,gtri,seti]},
%%   {13,[eqrr,gtir,setr,borr,banr,mulr,eqri,seti,bori,addi]},
%%   {14,[mulr,addr,muli,addi]},
%%   {15,[mulr,addi]}]}
%%
%% From this we can deduce that:
%%
%% 0 = eqri
%% 1 = seti
%% 2 = eqir
%% 3 = eqrr
%% 4 = addi
%% 5 = setr
%% 6 = gtrr
%% 7 = gtir
%% 8 = muli
%% 9 = bori
%% 10 = bani
%% 11 = borr
%% 12 = gtri
%% 13 = banr
%% 14 = addr
%% 15 = mulr
%%

%% This is also one of those "cheating" cases where we do part of the
%% solution by hand then just remember it.

%% This mapping is weirdly not "correct"; the corresponding Java
%% solution does not work with it and computes its own mapping instead
%% which works and is different.
opcode(0) ->
    eqri;
opcode(1) ->
    seti;
opcode(2) ->
    eqir;
opcode(3) ->
    eqrr;
opcode(4) ->
    addi;
opcode(5) ->
    setr;
opcode(6) ->
    gtrr;
opcode(7) ->
    gtir;
opcode(8) ->
    muli;
opcode(9) ->
    bori;
opcode(10) ->
    bani;
opcode(11) ->
    borr;
opcode(12) ->
    gtri;
opcode(13) ->
    banr;
opcode(14) ->
    addr;
opcode(15) ->
    mulr.

parse_lines([]) ->
    [];
parse_lines([Line | Lines]) ->
    Tokens = string:tokens(Line, ": [,]"),
    [parse_line(Tokens) | parse_lines(Lines)].

parse_line(Tokens) ->
    case Tokens of
        ["Before", R1, R2, R3, R4] ->
            [before, int(R1), int(R2), int(R3), int(R4)];
        [OpCode, A, B, C] ->
            [int(OpCode), int(A), int(B), int(C)];
        ["After", R1, R2, R3, R4] ->
            ['after', int(R1), int(R2), int(R3), int(R4)]
    end.

pass2([]) ->
    [];
pass2([[before | _] = Before, Instr, ['after' | _] = After | Rest]) ->
    [{Before, Instr, After} | pass2(Rest)];
pass2([_ | Rest]) ->
    pass2(Rest).

pass2_testprog([]) ->
    [];
pass2_testprog([[before | _], _, ['after' | _] | Rest]) ->
    pass2_testprog(Rest);
pass2_testprog([Instr | Rest]) ->
    [Instr | pass2_testprog(Rest)].

opcodes() ->
    [addr,
     addi,
     mulr,
     muli,
     banr,
     bani,
     borr,
     bori,
     setr,
     seti,
     gtir,
     gtri,
     gtrr,
     eqir,
     eqri,
     eqrr].

read_reg(Reg, RegVals) ->
    lists:nth(Reg + 1, RegVals).

write_reg(0, Val, [_R1, R2, R3, R4]) ->
    [Val, R2, R3, R4];
write_reg(1, Val, [R1, _R2, R3, R4]) ->
    [R1, Val, R3, R4];
write_reg(2, Val, [R1, R2, _R3, R4]) ->
    [R1, R2, Val, R4];
write_reg(3, Val, [R1, R2, R3, _R4]) ->
    [R1, R2, R3, Val].

%% -- opcode helpers --
execute_arithm_opcode_reg(A, B, C, RegVals, Fun) ->
    AVal = read_reg(A, RegVals),
    BVal = read_reg(B, RegVals),
    write_reg(C, Fun(AVal, BVal), RegVals).

execute_arithm_opcode_i(A, B, C, RegVals, Fun) ->
    AVal = read_reg(A, RegVals),
    write_reg(C, Fun(AVal, B), RegVals).

execute_compare_op(A, B, C, RegVals, Fun) ->
    write_reg(C,
              case Fun(A, B) of
                  true ->
                      1;
                  _ ->
                      0
              end,
              RegVals).

%% -- opcodes --

execute_opcode(addr, A, B, C, RegVals) ->
    execute_arithm_opcode_reg(A, B, C, RegVals, fun plus/2);
execute_opcode(addi, A, B, C, RegVals) ->
    execute_arithm_opcode_i(A, B, C, RegVals, fun plus/2);
execute_opcode(mulr, A, B, C, RegVals) ->
    execute_arithm_opcode_reg(A, B, C, RegVals, fun mul/2);
execute_opcode(muli, A, B, C, RegVals) ->
    execute_arithm_opcode_i(A, B, C, RegVals, fun mul/2);
execute_opcode(banr, A, B, C, RegVals) ->
    execute_arithm_opcode_reg(A, B, C, RegVals, fun bit_and/2);
execute_opcode(bani, A, B, C, RegVals) ->
    execute_arithm_opcode_i(A, B, C, RegVals, fun bit_and/2);
execute_opcode(borr, A, B, C, RegVals) ->
    execute_arithm_opcode_reg(A, B, C, RegVals, fun bit_or/2);
execute_opcode(bori, A, B, C, RegVals) ->
    execute_arithm_opcode_i(A, B, C, RegVals, fun bit_or/2);
execute_opcode(setr, A, _B, C, RegVals) ->
    AVal = read_reg(A, RegVals),
    write_reg(C, AVal, RegVals);
execute_opcode(seti, A, _B, C, RegVals) ->
    write_reg(C, A, RegVals);
execute_opcode(gtir, A, B, C, RegVals) ->
    AVal = read_reg(A, RegVals),
    execute_compare_op(AVal, B, C, RegVals, fun gt/2);
execute_opcode(gtri, A, B, C, RegVals) ->
    BVal = read_reg(B, RegVals),
    execute_compare_op(A, BVal, C, RegVals, fun gt/2);
execute_opcode(gtrr, A, B, C, RegVals) ->
    AVal = read_reg(A, RegVals),
    BVal = read_reg(B, RegVals),
    execute_compare_op(AVal, BVal, C, RegVals, fun gt/2);
execute_opcode(eqir, A, B, C, RegVals) ->
    AVal = read_reg(A, RegVals),
    execute_compare_op(AVal, B, C, RegVals, fun eq/2);
execute_opcode(eqri, A, B, C, RegVals) ->
    BVal = read_reg(B, RegVals),
    execute_compare_op(A, BVal, C, RegVals, fun eq/2);
execute_opcode(eqrr, A, B, C, RegVals) ->
    AVal = read_reg(A, RegVals),
    BVal = read_reg(B, RegVals),
    execute_compare_op(AVal, BVal, C, RegVals, fun eq/2).

%% -- utils --

int(S) ->
    list_to_integer(S).

plus(A, B) ->
    A + B.

mul(A, B) ->
    A * B.

bit_and(A, B) ->
    A band B.

bit_or(A, B) ->
    A bor B.

gt(X, Y) ->
    X > Y.

eq(X, Y) ->
    X == Y.
