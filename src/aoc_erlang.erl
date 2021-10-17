-module(aoc_erlang).

-export([main/1]).

main(Modules) ->
    lists:foreach(fun(Str) ->
                     M = list_to_atom(Str),
                     PI = aoc_puzzle:info(M),
                     io:format("~s... ", [aoc_puzzle:label(PI)]),
                     {Time, _} = timer:tc(fun() -> ok = aoc_puzzle:run_puzzle(PI) end),
                     io:format(" ~ps~n", [Time / 1000000.0])
                  end,
                  Modules).
