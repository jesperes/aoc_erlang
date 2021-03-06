-module(aoc2017_day24).

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 24,
                name = "Electromagnetic Moat",
                expected = {2006, 1994},
                has_input_file = true,
                use_one_solver_fun = true}.

-type input_type() :: [{integer(), integer()}].
-type result_type() :: {integer(), integer()}.

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    lists:map(fun(Line) ->
                 [A, B] = string:tokens(Line, "/"),
                 {list_to_integer(A), list_to_integer(B)}
              end,
              string:tokens(binary_to_list(Binary), "\n\r")).

-spec solve(Input :: input_type()) -> result_type().
solve(Components) ->
    Bridges = find_all_bridges0(0, Components),
    P1 = lists:max(
             lists:map(fun strength/1, Bridges)),
    {_, P2} =
        lists:max(
            lists:map(fun length_strength/1, Bridges)),
    {P1, P2}.

strength(Bridge) ->
    lists:foldl(fun({A, B}, Acc) -> Acc + A + B end, 0, Bridge).

length_strength(Bridge) ->
    {length(Bridge), strength(Bridge)}.

find_all_bridges0(First, Comps) ->
    lists:foldl(fun(Comp, Acc) ->
                   case matches(First, Comp) of
                       Other when is_integer(Other) ->
                           lists:foldl(fun(SubBridge, Acc0) -> [[Comp | SubBridge] | Acc0] end,
                                       Acc,
                                       lists:usort(find_all_bridges0(Other, Comps -- [Comp])));
                       false -> [[] | Acc]
                   end
                end,
                [],
                Comps).

matches(X, {Y, X}) when is_integer(X) ->
    Y;
matches(X, {X, Y}) when is_integer(X) ->
    Y;
matches(_, _) ->
    false.
