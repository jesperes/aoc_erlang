%%% Advent of Code solution for 2019 day 14.
%%% Created: 2019-12-14T07:03:21+00:00

-module(aoc2019_day14).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 14,
                name = "Space Stoichiometry",
                expected = {741927, 2371699},
                has_input_file = true}.

-type input_type() :: map().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    Lines = string:tokens(binary_to_list(Binary), "\n"),
    maps:from_list(
        lists:map(fun(Line) ->
                     [Left, Right] = string:tokens(Line, "=>"),
                     {_Q, C} = parse_chemical(Right),
                     {C,
                      {lists:map(fun parse_chemical/1, string:tokens(Left, ",")),
                       produces,
                       parse_chemical(Right)}}
                  end,
                  Lines)).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    produce(1, 'FUEL', Input).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    MaxOre = 1000000000000,
    binary_search(1, inf, MaxOre, Input).

%% Binary search exploring the upper limit by starting at 1 and
%% doubling until we overshoot.
binary_search(Lower, inf = Upper, MaxOre, Rules) ->
    case produce(Lower, 'FUEL', Rules) of
        TotalOre when TotalOre < MaxOre ->
            binary_search(Lower * 2, Upper, MaxOre, Rules);
        _ ->
            binary_search(floor(Lower / 2), Lower, MaxOre, Rules)
    end;
binary_search(Lower, Upper, MaxOre, Rules) when Lower < Upper ->
    case floor(Lower + (Upper - Lower) / 2) of
        %% If middle == lower, it means that upper exceed the limit, but
        %% middle does not.
        Middle when Middle == Lower ->
            Middle;
        Middle ->
            TotalOre = produce(Middle, 'FUEL', Rules),
            if TotalOre > MaxOre ->
                   binary_search(Lower, Middle, MaxOre, Rules);
               true ->
                   binary_search(Middle, Upper, MaxOre, Rules)
            end
    end.

parse_chemical(Str) ->
    [Quantity, Chemical] = string:tokens(Str, " "),
    {list_to_integer(Quantity), list_to_atom(Chemical)}.

%% Produce a given amount of chemical. Returns the total amount of ORE
%% used.
produce(Qnty, Chem, Rules) ->
    Inv0 = do_produce({Qnty, Chem}, Rules, #{}),
    maps:get(total_ore, Inv0).

%% Recursive entry point. Produce (at least) the given amount of
%% chemical given an inventory.
do_produce({Qnty, 'ORE'}, _, Inv) ->
    Inv0 = maps:update_with('ORE', fun(Old) -> Old + Qnty end, Qnty, Inv),
    Inv1 = maps:update_with(total_ore, fun(Old) -> Old + Qnty end, Qnty, Inv0),
    Inv1;
do_produce({Qnty, Chem}, Rules, Inv) ->
    case maps:get(Chem, Inv, 0) of
        Available when Available >= Qnty ->
            Inv;
        Available ->
            {_, produces, {Q, _}} = Rule = maps:get(Chem, Rules),
            Needed = Qnty - Available,
            Repeats = ceil(Needed / Q),
            apply_rule(Rule, Repeats, Rules, Inv)
    end.

%% Apply a rule to produce a chemical
%% @param Rule       The rule to produce
%% @param Multiplier How many copies of the rule to apply
%% @param Rules      The rules, needed to recurse when producing inputs.
%% @param Inv        Inventory
apply_rule({Inputs, produces, {Q, C}} = Rule, Multiplier, Rules, Inv) ->
    M = fun(X) -> X * Multiplier end,
    case lists:all(fun({Q0, C0}) -> maps:get(C0, Inv, 0) >= M(Q0) end, Inputs) of
        true ->
            Inv0 =
                lists:foldl(fun({Q0, C0}, Acc) -> maps:update_with(C0, fun(V) -> V - M(Q0) end, Acc)
                            end,
                            Inv,
                            Inputs),
            maps:update_with(C, fun(V) -> V + M(Q) end, M(Q), Inv0);
        false ->
            Inv0 =
                lists:foldl(fun({Qin, Cin}, Acc) -> do_produce({M(Qin), Cin}, Rules, Acc) end,
                            Inv,
                            Inputs),
            apply_rule(Rule, Multiplier, Rules, Inv0)
    end.
