-module(aoc2021_day24).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-compile([export_all, nowarn_export_all]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 24,
                name = "Arithmetic Logic Unit",
                expected = {0, 0},
                has_input_file = true}.

-type input_type() :: any().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    Lines = binary:split(Binary, <<"\n">>, [trim_all, global]),
    lists:map(fun(B) ->
                 case binary:split(B, <<" ">>, [trim_all, global]) of
                     [Instr, Op] -> {binary_to_atom(Instr), binary_to_atom(Op)};
                     [Instr, Op1, <<C, _/binary>> = Op2] when C >= $a andalso C =< $z ->
                         {binary_to_atom(Instr), binary_to_atom(Op1), binary_to_atom(Op2)};
                     [Instr, Op1, Op2] ->
                         {binary_to_atom(Instr), binary_to_atom(Op1), binary_to_integer(Op2)}
                 end
              end,
              Lines).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Prog) ->
    Segments = split_program(Prog, 0),
    % lists:foreach(fun(S) ->
    %                  Params = segm(S),
    %                  io:format(standard_error, "~p~n", [Params])
    %               end,
    %               Segments),
    alu_check(Prog),
    alu_check2(Prog),
    solve_dfs(Segments, 0, #{}, [], {inf, []}),
    0.

solve_dfs([], 0, Cache, Ws, {0, MaxWs}) ->
    {Cache, {0, max(MaxWs, Ws)}};
solve_dfs([], Z, Cache, Ws, {BestZ, MaxWs}) ->
    ?_if(Z < BestZ,
         begin
             erlang:display({Ws, BestZ, MaxWs}),
             {Cache, {Z, Ws}}
         end,
         {Cache, {BestZ, MaxWs}});
solve_dfs([{Sid, S} | Segments], Z, Cache, Ws, MaxWs) ->
    lists:foldl(fun(W, {CacheIn, MaxWsIn}) ->
                   case maps:get({Sid, W, Z}, CacheIn, undef) of
                       undef ->
                           Z1 = run_alu_segment(S, W, Z),
                           Cache0 = maps:put({Sid, W, Z}, Z1, CacheIn),
                           solve_dfs(Segments, Z1, Cache0, Ws ++ [W], MaxWsIn);
                       _ -> {CacheIn, MaxWsIn}
                   end
                end,
                {Cache, MaxWs},
                lists:seq(9, 1, -1)).

% [8,9,9,1,3,9,4,9,2,9,3,9,8,9]

-spec solve2(Input :: input_type()) -> result_type().
solve2(_Input) ->
    0.

% Segments can be parametrized on this pattern:
segm([{mul, x, 0},
      {add, x, z},
      {mod, x, 26},
      {'div', z, A},
      {add, x, B},
      {eql, x, w},
      {eql, x, 0},
      {mul, y, 0},
      {add, y, 25},
      {mul, y, x},
      {add, y, 1},
      {mul, z, y},
      {mul, y, 0},
      {add, y, w},
      {add, y, C},
      {mul, y, x},
      {add, z, y}]) ->
    {A, B, C}.

segment(Z, W, A, B, C) ->
    Z0 = Z div A,
    case Z rem 26 + B /= W of
        true ->
            Z0 * 26 + (W + C);
        false ->
            Z0
    end.

%% --------------------------------------------------------------------------
%% Utilities
%% --------------------------------------------------------------------------

split_program([], _) ->
    [];
split_program(Prog, N) ->
    {Segment, Rest} = split_one_segment(Prog, []),
    [{N, Segment} | split_program(Rest, N + 1)].

split_one_segment([{add, z, _} = Instr | Rest], Acc) ->
    {lists:reverse([Instr | Acc]), Rest};
split_one_segment([{inp, _} | L], Acc) ->
    split_one_segment(L, Acc);
split_one_segment([Instr | Rest], Acc) ->
    split_one_segment(Rest, [Instr | Acc]).

% Run one alu segment using the given "w" and "z" values as input. Returns the
% value of Z at the end of the segment.
run_alu_segment(Segment, W, Z) ->
    do_run_alu_segment(Segment, #{z => Z, w => W}).

do_run_alu_segment([], #{z := Z}) ->
    Z;
do_run_alu_segment([{Instr, Var, Op} | Rest], Vars) ->
    do_run_alu_segment(Rest, do_op(Instr, Var, Op, Vars)).

int_to_digit_list(Num) ->
    lists:map(fun(N) -> N - $0 end, integer_to_list(Num)).

run_full_alu_program(Prog, InputNum) ->
    Segments = split_program(Prog, 0),
    IntList = int_to_digit_list(InputNum),
    lists:foldl(fun({Digit, {_, Segment}}, ZAcc) -> run_alu_segment(Segment, Digit, ZAcc) end,
                0,
                lists:zip(IntList, Segments)).

run_full_alu_program2(Prog, InputNum) ->
    Segments = split_program(Prog, 0),
    IntList = int_to_digit_list(InputNum),
    lists:foldl(fun({Digit, {_, Segment}}, ZAcc) ->
                   {A, B, C} = segm(Segment),
                   ZAccOut = segment(ZAcc, Digit, A, B, C),
                   % io:format(standard_error, "~p + ~p -> ~p~n", [{A, B, C}, Digit, ZAccOut]),
                   ZAccOut
                end,
                0,
                lists:zip(IntList, Segments)).

opfuns(add) ->
    fun erlang:'+'/2;
opfuns(mul) ->
    fun erlang:'*'/2;
opfuns('div') ->
    fun erlang:'div'/2;
opfuns(mod) ->
    fun erlang:'rem'/2.

read_op(Op, Vars) when is_atom(Op) ->
    maps:get(Op, Vars, 0);
read_op(Op, _) ->
    Op.

do_op(eql, Var, Op, Vars) ->
    maps:put(Var, ?_if(maps:get(Var, Vars, 0) == read_op(Op, Vars), 1, 0), Vars);
do_op(Instr, Var, Op, Vars) ->
    maps:put(Var, (opfuns(Instr))(maps:get(Var, Vars, 0), read_op(Op, Vars)), Vars).

% Test inputs generated by a plain interpretation of the entire ALU code. Only
% used for testing.
test_inputs() ->
    [{83363573375981, 2939888656},
     {93587398886534, 3249787469},
     {67642494544722, 2368650457},
     {96115166799247, 3282196238},
     {48552955189425, 1763160536},
     {14438875253784, 788078557},
     {92527997439194, 3235168189},
     {13144712228158, 776583181},
     {32996174815598, 1384849489},
     {97994547818964, 3297718573},
     {55196452284217, 78397193},
     {53986513292743, 2014107984},
     {56568443767374, 2048872767},
     {61136458324533, 2296975478},
     {73847286918621, 2630127360},
     {55665446222412, 2036938531},
     {55918134154346, 78390073},
     {27925611911472, 1132127585},
     {86529269714678, 2973808419}].

alu_check(Prog) ->
    lists:foreach(fun({RandInt, ExpectedZ}) ->
                     ?assertEqual(ExpectedZ, run_full_alu_program(Prog, RandInt))
                  end,
                  test_inputs()).

alu_check2(Prog) ->
    lists:foreach(fun({RandInt, ExpectedZ}) ->
                     ?assertEqual(ExpectedZ, run_full_alu_program2(Prog, RandInt))
                  end,
                  test_inputs()).
