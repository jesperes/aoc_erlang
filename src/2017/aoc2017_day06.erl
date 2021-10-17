-module(aoc2017_day06).

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 6,
                name = "Memory Reallocation",
                expected = {3156, 1610},
                has_input_file = false,
                use_one_solver_fun = true}.

%% A set of memory banks is represented as a map of 0-based bankpositions to the amount of
%% blocks present in that memory bank.
-type memory_banks() :: #{Pos :: non_neg_integer() := NumBlocks :: non_neg_integer()}.

-type input_type() :: memory_banks().
-type result_type() :: {integer(), integer()}.

-spec parse(Binary :: binary()) -> input_type().
parse(_Binary) ->
    Banks = [2, 8, 8, 5, 4, 2, 3, 1, 5, 5, 1, 2, 15, 13, 5, 14],
    maps:from_list(
        lists:zip(
            lists:seq(0, length(Banks) - 1), Banks)).

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    {P1, FirstRepeat} = redist_until_repeat(Input, 0, sets:new()),
    {P2, _} = redist_until_repeat(FirstRepeat, 0, sets:new()),
    {P1, P2}.

%% Redistributed banks until we see a repeat. Return a tuple of {Cycles, Banks},
%% where Cycles is the number of redistribution cycles needed before
%% finding a loop, and the second is the first bank-configuration which repeated.
-spec redist_until_repeat(Banks :: input_type(),
                          N :: integer(),
                          Seen :: sets:set(input_type())) ->
                             {Cycles :: integer(), Banks :: input_type()}.
redist_until_repeat(Banks, N, Seen) ->
    case sets:is_element(Banks, Seen) of
        true ->
            {N, Banks};
        false ->
            NewBanks = redistribute(Banks),
            NewSeen = sets:add_element(Banks, Seen),
            redist_until_repeat(NewBanks, N + 1, NewSeen)
    end.

-spec redistribute(input_type()) -> input_type().
redistribute(Banks) ->
    {Pos, Amount} = find_largest(Banks),
    Banks0 = Banks#{Pos := 0},
    redistribute(Pos + 1, Amount, Banks0).

redistribute(_, 0, Banks) ->
    Banks;
redistribute(CurrPos, RemainingAmount, Banks) ->
    ActualCurrPos = CurrPos rem maps:size(Banks),
    Banks0 = maps:update_with(ActualCurrPos, fun(Old) -> Old + 1 end, Banks),
    redistribute(CurrPos + 1, RemainingAmount - 1, Banks0).

%% Return the largest memory block.
-spec find_largest(input_type()) -> {Pos :: integer(), NumBlocks :: integer()}.
find_largest(Banks) ->
    maps:fold(fun (K, V, undef) ->
                      {K, V};
                  (K, V, {_, VMax}) when V > VMax ->
                      {K, V};
                  (_K, _V, Acc) ->
                      Acc
              end,
              undef,
              Banks).
