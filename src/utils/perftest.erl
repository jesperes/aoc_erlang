-module(perftest).

-export([test/1, test/2]).

-define(SCHEDULER_REDUCTIONS_LIMIT, 2000).
-define(MIN_INNER_LOOPS, 20).
-define(STOP_OUTER_MSG, stop_outer_loop).

test(Fs) ->
    test(Fs, timer:seconds(1)).

test(Fs, TimeLimit) ->
    NamedFs = [{"empty loop", fun test_inner/1, []}] ++ lists:map(fun preprocess_f/1, Fs),
    MethodColWidth = lists:max([length(Name) || {Name, _, _} <- NamedFs]),
    io:format("~*s ~26s ~17s ~17s ~17s ~17s~n",
              [-MethodColWidth, "Method", "Loops", "Min", "Max", "Median", "Average"]),
    [test(Name, MethodColWidth, TimeLimit, Fun, Args) || {Name, Fun, Args} <- NamedFs],
    ok.

test(Name, MethodColWidth, TimeLimit, Fun, Args) ->
    InnerLoops = calc_inner_loops(Fun, Args),
    {ok, _} = timer:send_after(TimeLimit, ?STOP_OUTER_MSG),
    Stats = lists:sort(test_outer(InnerLoops, Fun, Args)),
    OuterLoops = length(Stats),
    Min = hd(Stats),
    Max = lists:last(Stats),
    Med = case OuterLoops div 2 of
              X when X > 0 ->
                  lists:nth(X, Stats);
              _ ->
                  0.0
          end,
    Avg = lists:sum(Stats) / OuterLoops,
    TotalLoops = OuterLoops * InnerLoops,
    io:format("~*s ~7B * ~4B = ~9B ~15.3fus ~15.3fus ~15.3fus ~15.3fus~n",
              [-MethodColWidth, Name, OuterLoops, InnerLoops, TotalLoops, Min, Max, Med, Avg]),
    {Name, Min, Max, Med, Avg}.

test_outer(InnerLoops, F, Args) ->
    receive
        ?STOP_OUTER_MSG ->
            []
    after 0 ->
        {Pid, Mon} =
            spawn_monitor(fun() ->
                             {T, _} = timer:tc(F, [InnerLoops | Args]),
                             exit(T)
                          end),
        receive
            {'DOWN', Mon, process, Pid, T} when is_integer(T) ->
                [T / InnerLoops | test_outer(InnerLoops, F, Args)];
            {'DOWN', Mon, process, Pid, R} ->
                exit(R)
        end
    end.

test_inner(1, F, Args) ->
    apply(F, Args);
test_inner(N, F, Args) ->
    apply(F, Args),
    test_inner(N - 1, F, Args).

test_inner(1) ->
    ok;
test_inner(N) ->
    test_inner(N - 1).

preprocess_f(F) when is_function(F, 0) ->
    preprocess_f({F, []});
preprocess_f({M, F, Args}) when is_atom(M), is_atom(F), is_list(Args) ->
    A = length(Args),
    preprocess_f({fun M:F/A, Args});
preprocess_f({F, Args}) when is_function(F), is_list(Args) ->
    {module, Module} = erlang:fun_info(F, module),
    {name, Name} = erlang:fun_info(F, name),
    ArgsStr = lists:join(", ", lists:map(fun io_lib:print/1, Args)),
    NameStr =
        lists:flatten(
            io_lib:format("~w:~w(~s)", [Module, Name, ArgsStr])),
    {NameStr, fun test_inner/3, [F, Args]}.

calc_inner_loops(F, Args) ->
    S = self(),
    {reductions, R1} = process_info(S, reductions),
    apply(F, [1 | Args]),
    {reductions, R2} = process_info(S, reductions),
    max(?MIN_INNER_LOOPS, ?SCHEDULER_REDUCTIONS_LIMIT div (R2 - R1)).
