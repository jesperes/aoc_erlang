-module(aoc2017_day08).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 8,
                name = "I Heard You Like Registers",
                expected = {6611, 6619},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: [string()].
-type result_type() :: {integer(), integer()}.

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    string:tokens(binary_to_list(Binary), "\r\n").

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    {Max, StateOut} =
        lists:foldl(fun(Line, {Max, State}) ->
                       case string:tokens(Line, " ") of
                           [DestReg, Op1, Val1, "if", Reg2, Op2, Val2] ->
                               Reg2Val = maps:get(Reg2, State, 0),
                               Val2Val = list_to_integer(Val2),
                               OpRes =
                                   case Op2 of
                                       "<" -> Reg2Val < Val2Val;
                                       ">" -> Reg2Val > Val2Val;
                                       ">=" -> Reg2Val >= Val2Val;
                                       "==" -> Reg2Val == Val2Val;
                                       "!=" -> Reg2Val =/= Val2Val;
                                       "<=" -> Reg2Val =< Val2Val
                                   end,

                               State0 =
                                   case OpRes of
                                       true ->
                                           case Op1 of
                                               "inc" -> inc(DestReg, list_to_integer(Val1), State);
                                               "dec" -> dec(DestReg, list_to_integer(Val1), State)
                                           end;
                                       false -> State
                                   end,
                               {max(Max, maps:get(DestReg, State0, 0)), State0}
                       end
                    end,
                    {0, #{}},
                    Input),

    Part1 =
        lists:max(
            maps:values(StateOut)),
    {Part1, Max}.

inc(Reg, Val, State) ->
    maps:update_with(Reg, fun(Old) -> Old + Val end, Val, State).

dec(Reg, Val, State) ->
    maps:update_with(Reg, fun(Old) -> Old - Val end, -Val, State).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
