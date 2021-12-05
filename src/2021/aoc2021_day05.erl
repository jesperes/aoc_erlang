-module(aoc2021_day05).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 5,
                name = "Hydrothermal Venture",
                expected = {4728, 17717},
                has_input_file = true}.

-type input_type() :: any().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    lists:map(fun(B) ->
                 lists:map(fun list_to_integer/1, string:tokens(binary_to_list(B), ", ->"))
              end,
              binary:split(Binary, <<"\n">>, [trim_all, global])).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    solve(Input, false).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    solve(Input, true).

solve(Input, DoDiag) ->
    Points = lists:foldl(fun(Coords, Map) -> draw_line(Coords, DoDiag, Map) end, #{}, Input),
    count_overlaps(Points).

%% Helpers

count_overlaps(Map) ->
    maps:fold(fun (_K, V, Acc) when V >= 2 ->
                      Acc + 1;
                  (_K, _V, Acc) ->
                      Acc
              end,
              0,
              Map).

draw_line([X0, Y0, X1, Y1], DoDiag, Map) ->
    case {X0 == X1, Y0 == Y1} of
        {true, false} ->
            lists:foldl(fun(Y, InnerMap) -> inc({X0, Y}, InnerMap) end, Map, seq(Y0, Y1));
        {false, true} ->
            lists:foldl(fun(X, InnerMap) -> inc({X, Y0}, InnerMap) end, Map, seq(X0, X1));
        _ when DoDiag ->
            Dx = sign(X1 - X0),
            Dy = sign(Y1 - Y0),
            Len = abs(X1 - X0),
            lists:foldl(fun(L, InnerMap) ->
                           X = X0 + L * Dx,
                           Y = Y0 + L * Dy,
                           inc({X, Y}, InnerMap)
                        end,
                        Map,
                        seq(0, Len));
        _ ->
            Map
    end.

inc(Key, Map) ->
    maps:update_with(Key, fun(Old) -> Old + 1 end, 1, Map).

sign(X) when X < 0 ->
    -1;
sign(X) when X == 0 ->
    0;
sign(_) ->
    1.

seq(X, Y) when X =< Y ->
    lists:seq(X, Y);
seq(X, Y) when X > Y ->
    lists:seq(Y, X).
