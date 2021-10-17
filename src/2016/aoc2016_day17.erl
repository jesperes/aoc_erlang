-module(aoc2016_day17).

-behavior(aoc_puzzle).

%% dist/2 is used as a search callback, but doesn't use its arguments.
-hank([{unnecessary_function_arguments, [{dist, 2}]}]).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 17,
                name = "Two Steps Forward",
                expected = {"DDRRUDLRRD", 488},
                has_input_file = false}.

-type input_type() :: string().
-type result1_type() :: string().
-type result2_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    "pslxynzg".

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    {_, [{_, _, Solution} | _]} = start(Input),
    Solution.

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    start_part2(Input).

start(Passcode) ->
    astar2:astar({0, 0, ""},
                 fun is_end/1,
                 fun cost/1,
                 fun(Node) -> neighbors(Node, Passcode) end,
                 fun dist/2).

start_part2(Passcode) ->
    dfs({0, 0, ""}, Passcode, 0).

dfs({3, 3, Path}, _, Acc) ->
    Len = length(Path),
    if Len > Acc ->
           Len;
       true ->
           Acc
    end;
dfs({_, _, _Path} = Node, Passcode, Acc) ->
    lists:foldl(fun(Nbr, AccIn) -> dfs(Nbr, Passcode, AccIn) end,
                Acc,
                neighbors(Node, Passcode)).

%%% Search callbacks
is_end({3, 3, _}) ->
    true;
is_end(_) ->
    false.

cost({X, Y, _}) ->
    abs(X - 3) + abs(Y - 3).

dist(_, _) ->
    1.

neighbors({X, Y, Path}, Passcode) ->
    [U, D, L, R | _] = md5(Passcode ++ Path),
    L1 = add_if_open(U, {X, Y - 1}, $U, Path, []),
    L2 = add_if_open(D, {X, Y + 1}, $D, Path, L1),
    L3 = add_if_open(L, {X - 1, Y}, $L, Path, L2),
    add_if_open(R, {X + 1, Y}, $R, Path, L3).

add_if_open(Char, {X, Y}, Dir, Path, List) ->
    if Char >= $b, X >= 0, Y >= 0, X =< 3, Y =< 3 ->
           [{X, Y, Path ++ [Dir]} | List];
       true ->
           List
    end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b", [N]) || <<N>> <= erlang:md5(S)]).
