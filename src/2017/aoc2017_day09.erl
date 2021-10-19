-module(aoc2017_day09).

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 9,
                name = "Stream Processing",
                expected = {7616, 3838},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: string().
-type result_type() :: {integer(), integer()}.

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    string:trim(binary_to_list(Binary)).

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    group_score(Input).

group_score(Input) ->
    {Score, _, Garbage} = group_score(Input, 0, 0),
    {Score, Garbage}.

group_score([], Score, Garbage) ->
    {Score, [], Garbage};
group_score([${ | G], Score, Garbage) ->
    {Score1, Rest, Garbage1} = group_score(G, Score + 1, Garbage),
    {Score2, Rest2, Garbage2} = group_score(Rest, Score, Garbage1),
    {Score1 + Score2, Rest2, Garbage2};
group_score([$} | G], Score, Garbage) ->
    {Score, G, Garbage};
group_score([$, | G], Score, Garbage) ->
    group_score(G, Score, Garbage);
group_score([$< | G], Score, Garbage) ->
    {Rest, Garbage1} = swallow_garbage(G, Garbage),
    group_score(Rest, Score, Garbage1).

swallow_garbage([$> | G], Garbage) ->
    {G, Garbage};
swallow_garbage([$!, _ | G], Garbage) ->
    swallow_garbage(G, Garbage);
swallow_garbage([_ | G], Garbage) ->
    swallow_garbage(G, Garbage + 1).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

group_score_test() ->
    % ?assertEqual(1, group_score("{}")),
    % ?assertEqual(6, group_score("{{{}}}")),
    % ?assertEqual(5, group_score("{{},{}}")),
    % ?assertEqual(16, group_score("{{{},{},{{}}}}")),
    % ?assertEqual(9, group_score("{{<ab>},{<ab>},{<ab>},{<ab>}}")),
    % ?assertEqual(3, group_score("{{<a!>},{<a!>},{<a!>},{<ab>}}")).
    ?assertMatch({_, 10}, group_score("<{o\"i!a,<{i<a>")),
    ?assertMatch({_, 0}, group_score("<!!!>>")),
    ?assertMatch({_, 3}, group_score("<<<<>")),
    ?assertMatch({_, 2}, group_score("<{!>}>")).

-endif.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
