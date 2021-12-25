-module(aoc2021_day24).

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 24,
                name = "Arithmetic Logic Unit",
                expected = {89913949293989, 12911816171712},
                has_input_file = true,
                use_one_solver_fun = true}.

-type input_type() :: any().
-type result_type() :: integer().

% Stack machine state
-record(state,
        {dig = 0 :: integer,
         stack = [] :: list(),
         digits = #{} :: map(),
         push = false :: boolean(),
         sub = 0 :: integer()}).

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    Lines = binary:split(Binary, <<"\n">>, [trim_all, global]),
    lists:map(fun({I, B}) ->
                 {I,
                  case binary:split(B, <<" ">>, [trim_all, global]) of
                      [Instr, Op] -> {binary_to_atom(Instr), binary_to_atom(Op)};
                      [Instr, Op1, <<C, _/binary>> = Op2] when C >= $a andalso C =< $z ->
                          {binary_to_atom(Instr), binary_to_atom(Op1), binary_to_atom(Op2)};
                      [Instr, Op1, Op2] ->
                          {binary_to_atom(Instr), binary_to_atom(Op1), binary_to_integer(Op2)}
                  end}
              end,
              lists:zip(
                  lists:seq(0, length(Lines) - 1), Lines)).

-spec solve(Input :: input_type()) -> result_type().
solve(Prog) ->
    #state{digits = Digits} = lists:foldl(fun stack_machine/2, #state{}, Prog),
    digits_to_answer(Digits).

digits_to_answer(Digits) ->
    List = maps:to_list(Digits),
    SortedList = lists:sort(List),
    Vals = lists:map(fun({_, V}) -> V end, SortedList),
    {P2, P1} = lists:unzip(Vals),
    {intlist_to_int(P1), intlist_to_int(P2)}.

% The stack-machine trick was taken from
% https://www.reddit.com/r/adventofcode/comments/rnejv5/comment/hpv7g7j/
stack_machine({I, {_, _, Op}}, State) when I rem 18 == 4 ->
    State#state{push = Op == 1};
stack_machine({I, {_, _, Op}}, State) when I rem 18 == 5 ->
    State#state{sub = Op};
stack_machine({I, {_, _, Op}},
              #state{dig = Dig,
                     push = true,
                     stack = Stack} =
                  State)
    when I rem 18 == 15 ->
    State#state{stack = [{Dig, Op} | Stack], dig = Dig + 1};
stack_machine({I, _},
              #state{push = false,
                     sub = Sub,
                     dig = Dig,
                     stack = [{Sibling, Add} | Stack],
                     digits = Digits} =
                  State)
    when I rem 18 == 15 ->
    DigitsOut =
        case Add + Sub of
            Diff when Diff < 0 ->
                Digits0 = maps:put(Sibling, {-Diff + 1, 9}, Digits),
                maps:put(Dig, {1, 9 + Diff}, Digits0);
            Diff ->
                Digits0 = maps:put(Sibling, {1, 9 - Diff}, Digits),
                maps:put(Dig, {1 + Diff, 9}, Digits0)
        end,
    State#state{dig = Dig + 1,
                digits = DigitsOut,
                stack = Stack};
stack_machine(_, State) ->
    State.

intlist_to_int(List) ->
    list_to_integer(lists:map(fun(N) -> N + $0 end, List)).
