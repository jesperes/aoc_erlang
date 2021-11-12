-module(aoc_erlang).

-export([main/1]).

-include("aoc_puzzle.hrl").

options() ->
    [{prof, undefined, "prof", boolean, "Run with profiling"}].

main(Args) ->
    {ok, {Options, Regexps}} = getopt:parse(options(), Args),
    Regexps0 =
        case Regexps of
            [] ->
                ["aoc\\d+_day\\d+"];
            _ ->
                Regexps
        end,

    Modules =
        lists:usort(
            lists:filtermap(fun({Module, _, _}) ->
                               case lists:any(fun(RE) ->
                                                 case re:run(Module, RE) of
                                                     nomatch -> false;
                                                     _ -> true
                                                 end
                                              end,
                                              Regexps0)
                               of
                                   true -> {true, list_to_atom(Module)};
                                   false -> false
                               end
                            end,
                            code:all_available())),

    case proplists:get_value(prof, Options) of
        true ->
            io:format("Running puzzles with profiling enabled.~n", []),
            {ok, _} = eprof:start(),
            eprof:profile(fun() -> run_puzzles(Modules) end),
            eprof:stop_profiling(),
            ok = eprof:analyze();
        _ ->
            run_puzzles(Modules)
    end.

run_puzzles(Modules) ->
    io:format("Running ~p puzzles, please wait...~n", [length(Modules)]),
    {TotalTime, Lines} =
        timer:tc(fun() ->
                    lists:map(fun(M) ->
                                 PI = aoc_puzzle:info(M),
                                 {Time, _} = timer:tc(fun() -> ok = aoc_puzzle:run_puzzle(PI) end),
                                 [PI#aoc_puzzle.year,
                                  PI#aoc_puzzle.day,
                                  PI#aoc_puzzle.name,
                                  {time, Time}]
                              end,
                              Modules)
                 end),

    Lines0 = lists:sort(fun([_, _, _, T0], [_, _, _, T1]) -> T0 < T1 end, Lines),

    Lines1 =
        [["Year", "Day", "Name", "Time (msecs)"]]
        ++ Lines0
        ++ [["", "", "Total time", io_lib:format("~p secs", [TotalTime / 1000000.0])]],

    Lines2 =
        lists:map(fun(Line) ->
                     lists:map(fun (Field) when is_list(Field) -> Field;
                                   (Field) when is_integer(Field) -> integer_to_list(Field);
                                   ({time, Field}) -> io_lib:format("~p", [Field / 1000.0])
                               end,
                               Line)
                  end,
                  Lines1),

    Lines3 = lists:join("\n", lists:map(fun(Line) -> lists:join("@", Line) end, Lines2)),

    ok = file:write_file("table.txt", list_to_binary(Lines3)),
    Output = os:cmd("tabulate -1 -f simple -s @ table.txt"),
    io:format("~n~s", [Output]).
