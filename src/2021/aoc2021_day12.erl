-module(aoc2021_day12).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 12,
                name = "Passage Pathing",
                expected = {4549, 120535},
                has_input_file = true}.

-type input_type() :: digraph:graph().
-type result_type() :: any().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    Edges =
        lists:map(fun(B) ->
                     [X, Y] = binary:split(B, <<"-">>),
                     {btoa(X), btoa(Y)}
                  end,
                  binary:split(Binary, <<"\n">>, [trim_all, global])),
    G = digraph:new(),
    lists:foreach(fun({A, B}) ->
                     digraph:add_vertex(G, A),
                     digraph:add_vertex(G, B),
                     add_edge(G, A, B),
                     add_edge(G, B, A)
                  end,
                  Edges),
    G.

add_edge(_, _, start) ->
    ok;
add_edge(_, 'end', _) ->
    ok;
add_edge(G, X, Y) ->
    digraph:add_edge(G, X, Y).

-spec solve1(Input :: input_type()) -> result_type().
solve1(G) ->
    find_all_paths(start, G, #{}, 0, fun allow_small_cave_visit/3).

-spec solve2(Input :: input_type()) -> result_type().
solve2(G) ->
    find_all_paths(start, G, #{}, 0, fun allow_small_cave_visit2/3).

cave_size(Name) when Name > '_' ->
    small;
cave_size(_) ->
    large.

find_all_paths('end', _G, _State, NumPaths, _Fun) ->
    NumPaths + 1;
find_all_paths(Node, G, State, NumPaths, Fun) ->
    CaveSize = cave_size(Node),
    case {CaveSize, Fun(Node, CaveSize, State)} of
        {small, false} ->
            NumPaths;
        {small, true} ->
            %% For small caves, we update the freqency map
            State0 = maps:update_with(Node, fun(Old) -> Old + 1 end, 1, State),
            lists:foldl(fun(Nbr, Acc) -> find_all_paths(Nbr, G, State0, Acc, Fun) end,
                        NumPaths,
                        digraph:out_neighbours(G, Node));
        {large, _} ->
            lists:foldl(fun(Nbr, Acc) -> find_all_paths(Nbr, G, State, Acc, Fun) end,
                        NumPaths,
                        digraph:out_neighbours(G, Node))
    end.

%% Functions to determine whether we are allow to visit small caves.
allow_small_cave_visit(_, large, _) ->
    true;
allow_small_cave_visit(Node, _, Map) ->
    not maps:is_key(Node, Map).

allow_small_cave_visit2(_, large, _) ->
    true;
allow_small_cave_visit2(Node, _, Map) ->
    case max_value(Map) of
        Max when Max >= 2 ->
            case maps:get(Node, Map, 0) of
                Freq when Freq >= 1 ->
                    false;
                _ ->
                    true
            end;
        _ ->
            true
    end.

max_value(Map) ->
    case maps:values(Map) of
        [] ->
            0;
        Values ->
            lists:max(Values)
    end.

btoa(B) when is_binary(B) ->
    list_to_atom(binary_to_list(B)).

%% Tests

-ifdef(TEST).

ex1_test() ->
    Binary = <<"start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end\n">>,
    ?assertEqual(10, solve1(parse(Binary))),
    ?assertEqual(36, solve2(parse(Binary))).

-endif.
