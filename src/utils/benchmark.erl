-module(benchmark).

-export([benchmark/2]).

-include_lib("eunit/include/eunit.hrl").

benchmark(Msg, Fun) ->
    % io:format(standard_error, "~s: start~n", [Msg]),
    Reps = 10,
    Total =
        lists:foldl(fun(_, Sum) ->
                       {Time, _} = timer:tc(Fun),
                       Time + Sum
                    end,
                    0,
                    lists:seq(1, Reps)),
    UsecsPerIter = Total / Reps,
    io:format(standard_error, "~s: average=~p~n", [Msg, UsecsPerIter]),
    UsecsPerIter.

-ifdef(TEST).

% Benchmark two ways of splitting a file into lines
binary_split_lines_1_test() ->
    Binary =
        list_to_binary(lists:flatten(
                           lists:foldl(fun(N, Acc) -> [io_lib:format("~p~n", [N]) | Acc] end,
                                       [],
                                       lists:seq(1, 1000)))),

    benchmark("using binary:split/3",
              fun() ->
                 lists:map(fun binary_to_integer/1, binary:split(Binary, <<"\n">>, [global, trim]))
              end).

binary_split_lines_2_test() ->
    Binary =
        list_to_binary(lists:flatten(
                           lists:foldl(fun(N, Acc) -> [io_lib:format("~p~n", [N]) | Acc] end,
                                       [],
                                       lists:seq(1, 1000)))),

    benchmark("using string:tokens/3",
              fun() ->
                 lists:map(fun list_to_integer/1,
                           string:tokens(
                               string:trim(binary_to_list(Binary)), "\n"))
              end).

map_sext_encoding_test_() ->
    Size = 1000,
    Runs =
        [{map_tuple, fun(X, Y) -> {X, Y} end, fun({X, Y}) -> {X, Y} end},
         {map_list, fun(X, Y) -> [X, Y] end, fun([X, Y]) -> {X, Y} end},
         {map_bits, fun(X, Y) -> (X bsl 16) bor Y end, fun(P) -> {P bsr 16, P band 16#ffff} end},
         {map_binary16, fun(X, Y) -> <<X:16, Y:16>> end, fun(<<X:16, Y:16>>) -> {X, Y} end}],

    Tuples = [{X, Y} || X <- lists:seq(0, Size - 1), Y <- lists:seq(0, Size - 1)],
    ExpectedResult = lists:foldl(fun({X, Y}, Acc) -> X * Y + Acc end, 0, Tuples),
    lists:map(fun({Type, EncodeFun, DecodeFun}) ->
                 {timeout,
                  3600,
                  {atom_to_list(Type),
                   fun() ->
                      Coords =
                          [EncodeFun(X, Y)
                           || X <- lists:seq(0, Size - 1), Y <- lists:seq(0, Size - 1)],
                      Map = lists:foldl(fun(Coord, Map) ->
                                           {X, Y} = DecodeFun(Coord),
                                           maps:put(Coord, X * Y, Map)
                                        end,
                                        #{},
                                        Coords),
                      MapResult =
                          lists:foldl(fun(Coord, Acc) -> Acc + maps:get(Coord, Map) end, 0, Coords),
                      ?assertEqual(ExpectedResult, MapResult)
                   end}}
              end,
              Runs).

-endif.
