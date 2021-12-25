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

-type input_type() :: map().
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
    S = trans_alu_program(Prog),
    io:format(standard_error, "~s~n", [S]),
    check_numbers_at_random(Prog, 100),
    0.

-spec solve2(Input :: input_type()) -> result_type().
solve2(_Input) ->
    0.

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

ll(N) when is_integer(N) ->
    io_lib:format("~w", [N]);
ll(N) ->
    io_lib:format("~s", [N]).


trans_alu_program([]) ->
    [];
trans_alu_program([{inp, Var} | Rest]) ->
    io_lib:format("~s = read_next(inputs);~n", [Var]) ++ trans_alu_program(Rest);
%trans_alu_program([{'div', _, 1} | Rest]) ->
%    trans_alu_program(Rest);
%% Optimization rules
% trans_alu_program([{mul, Var, 0}, {add, Var, Var2} | Rest]) ->
%     io_lib:format("~s = ~w; // opt 1~n", [Var, Var2]) ++ trans_alu_program(Rest);
% trans_alu_program([{mul, Var, 0} | Rest]) ->
%     io_lib:format("~s = 0; // opt 2~n", [Var]) ++ trans_alu_program(Rest);
% trans_alu_program([{eql, Var1, Var2}, {eql, Var1, 0} | Rest]) ->
%     io_lib:format("~s = (~s != ~s) ? 1: 0; // opt 3~n", [Var1, Var1, Var2]) ++ trans_alu_program(Rest);
%% Default rules
trans_alu_program([{add, Var, Op} | Rest]) ->
    io_lib:format("~s += ~s;~n", [Var, ll(Op)]) ++ trans_alu_program(Rest);
trans_alu_program([{mul, Var, Op} | Rest]) ->
    io_lib:format("~s *= ~s;~n", [Var, ll(Op)]) ++ trans_alu_program(Rest);
trans_alu_program([{'div', Var, Op} | Rest]) ->
    io_lib:format("~s /= ~s;~n", [Var, ll(Op)]) ++ trans_alu_program(Rest);
trans_alu_program([{mod, Var, Op} | Rest]) ->
    io_lib:format("~s %= ~s;~n", [Var, ll(Op)]) ++ trans_alu_program(Rest);
trans_alu_program([{eql, Var, Op} | Rest]) ->
    io_lib:format("~s = (~s == ~s) ? 1 : 0;~n", [Var, ll(Var), ll(Op)]) ++ trans_alu_program(Rest).

run_alu_program([], Vars, _) ->
    Vars;
run_alu_program([{inp, Var} | Rest], Vars, [N | InputNums]) ->
    run_alu_program(Rest, maps:put(Var, N, Vars), InputNums);
run_alu_program([{eql, Var, Op} | Rest], Vars, InputNums) ->
    run_alu_program(Rest,
                    maps:put(Var,
                             case maps:get(Var, Vars, 0) == read_op(Op, Vars) of
                                 true ->
                                     1;
                                 false ->
                                     0
                             end,
                             Vars),
                    InputNums);
run_alu_program([{Instr, Var, Op} | Rest], Vars, InputNums) ->
    run_alu_program(Rest,
                    maps:put(Var, (opfuns(Instr))(maps:get(Var, Vars, 0), read_op(Op, Vars)), Vars),
                    InputNums).

int_to_digit_list(Num) ->
    lists:map(fun(N) -> N - $0 end, integer_to_list(Num)).

get_z(Prog, List) ->
    maps:get(z, run_alu_program(Prog, #{}, List)).

check_numbers_at_random(_Prog, 0) ->
    ok;
check_numbers_at_random(Prog, N) ->
    if N rem 10000 == 0 ->
           io:format(standard_error, "Checked ~p numbers at random~n", [N]);
       true ->
           ok
    end,
    RandInt = rand:uniform(99999999999999),
    List = int_to_digit_list(RandInt),
    case {length(List), lists:member(0, List)} of
        {14, false} ->
            case get_z(Prog, List) of
                0 ->
                    io:format(standard_error, "VALID:   ~p~n", [RandInt]);
                Z ->
                    io:format(standard_error,
                              "{~n  int inputs[] = {~s};~n  assert(~pLL == alu(inputs));~n}~n",
                              [string:join(
                                   lists:map(fun(X) -> [X] end, integer_to_list(RandInt)), ", "),
                               Z]),
                    ok
            end;
        _ ->
            ok
    end,
    check_numbers_at_random(Prog, N - 1).

%% Tests
%%
-ifdef(TEST).

ex1_test() ->
    ?assertMatch(#{x := -42}, run_alu_program([{inp, x}, {mul, x, -1}], #{}, [42])).

ex2_test() ->
    Prog =
        parse(<<"inp w\nadd z w\nmod z 2\ndiv w 2\nadd y w\nmod y 2\ndiv w 2\nadd "
                "x w\nmod x 2\ndiv w 2\nmod w 2">>),
    ?assertMatch(#{x := 1,
                   y := 1,
                   w := 1,
                   z := 1},
                 run_alu_program(Prog, #{}, [16#f])).

int_to_digit_list_test() ->
    ?assertEqual([1, 2, 3], int_to_digit_list(123)).

-endif.
