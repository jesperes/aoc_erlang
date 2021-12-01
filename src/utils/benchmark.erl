-module(benchmark).

-export([benchmark/2]).

-include_lib("eunit/include/eunit.hrl").

benchmark(Msg, Fun) ->
    Reps = 100,
    Total =
        lists:foldl(fun(_, Sum) ->
                       {Time, _} = timer:tc(Fun),
                       Time + Sum
                    end,
                    0,
                    lists:seq(1, Reps)),
    UsecsPerIter = Total / Reps,
    ?debugFmt("~s: average=~p", [Msg, UsecsPerIter]),
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

-endif.
