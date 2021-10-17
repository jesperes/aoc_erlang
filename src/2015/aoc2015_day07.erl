-module(aoc2015_day07).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 7,
                name = "Some Assembly Required",
                expected = {956, 40149},
                has_input_file = true}.

-type atom_or_int() :: atom() | integer().
-type input_type() :: [{atom_or_int()}].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    lists:map(fun(Line) -> list_to_tuple(lists:map(fun c/1, string:tokens(Line, " "))) end,
              string:tokens(binary_to_list(Input), "\n\r")).

-spec solve1(Lines :: input_type()) -> result1_type().
solve1(Lines) ->
    propagate_signals(Lines, #{}).

-spec solve2(Lines :: input_type()) -> result2_type().
solve2(Lines) ->
    {Part1Sol, _} = (info())#aoc_puzzle.expected,
    propagate_signals(Lines, #{b => Part1Sol}).

c(S) ->
    try
        list_to_integer(S)
    catch
        error:badarg ->
            list_to_atom(S)
    end.


-spec propagate_signals(List :: input_type(), Map :: map()) -> integer().
propagate_signals(List, Map) ->
    case maps:is_key(a, Map) of
        true ->
            maps:get(a, Map);
        false ->
            propagate_signals(List, run_pass(List, Map))
    end.

read_value(A, _Map) when is_integer(A) ->
    A;
read_value(A, Map) ->
    maps:get(A, Map, undefined).


-spec run_pass(input_type(), map()) -> map().
run_pass([], Map) ->
    Map;
run_pass([{WireA, '->', WireB} | Rest], Map) ->
    run_pass(Rest, do_unary_op(WireA, WireB, fun(A) -> A end, Map));
run_pass([{WireA, 'AND', WireB, _, WireC} | Rest], Map) ->
    run_pass(Rest, do_bin_op(WireA, WireB, WireC, fun(A, B) -> A band B end, Map));
run_pass([{WireA, 'OR', WireB, _, WireC} | Rest], Map) ->
    run_pass(Rest, do_bin_op(WireA, WireB, WireC, fun(A, B) -> A bor B end, Map));
run_pass([{'NOT', WireA, _, WireB} | Rest], Map) ->
    run_pass(Rest, do_unary_op(WireA, WireB, fun(A) -> bnot A end, Map));
run_pass([{WireA, 'LSHIFT', WireB, _, WireC} | Rest], Map) ->
    run_pass(Rest, do_bin_op(WireA, WireB, WireC, fun(A, B) -> A bsl B end, Map));
run_pass([{WireA, 'RSHIFT', WireB, _, WireC} | Rest], Map) ->
    run_pass(Rest, do_bin_op(WireA, WireB, WireC, fun(A, B) -> A bsr B end, Map)).

write_wire(Wire, Value, Map) ->
    maps:update_with(Wire, fun(V) -> V end, Value, Map).

do_unary_op(WireA, WireB, Fun, Map) ->
    A = read_value(WireA, Map),
    if A =:= undefined ->
           Map;
       true ->
           write_wire(WireB, Fun(A), Map)
    end.

do_bin_op(WireA, WireB, WireC, Fun, Map) ->
    A = read_value(WireA, Map),
    B = read_value(WireB, Map),
    if (A =:= undefined) or (B =:= undefined) ->
           Map;
       true ->
           write_wire(WireC, Fun(A, B), Map)
    end.
