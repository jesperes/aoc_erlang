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
    find_all_paths(G, start, #{}, 0, fun allow_small_cave_visit/2).

-spec solve2(Input :: input_type()) -> result_type().
solve2(G) ->
    find_all_paths(G, start, #{}, 0, fun allow_small_cave_visit2/2).

cave_size(start) ->
    large;
cave_size('end') ->
    large;
cave_size(Name) when Name =< 'ZZZ' ->
    large;
cave_size(_) ->
    small.

find_all_paths(_, 'end', _CurrentPath, NumPaths, _Fun) ->
    NumPaths + 1;
find_all_paths(G, Node, CurrentPath, NumPaths, Fun) ->
    case {Node, cave_size(Node), Fun(Node, CurrentPath)} of
        {_, small, false} ->
            NumPaths;
        {_, _, _F} ->
    %             NewCurrentPath = maps:update_with(
            lists:foldl(fun(Nbr, Acc) -> find_all_paths(G, Nbr, [Node | CurrentPath], Acc, Fun) end,
                        NumPaths,
                        digraph:out_neighbours(G, Node))
    end.

allow_small_cave_visit(Node, Path) ->
    not lists:member(Node, Path).

allow_small_cave_visit2(Node, Path) ->
    Map = lists:foldl(fun(N, Acc) ->
                         case cave_size(N) of
                             small -> maps:update_with(N, fun(Old) -> Old + 1 end, 1, Acc);
                             _ -> Acc
                         end
                      end,
                      #{},
                      Path),
    case maps:size(Map) of
        0 ->
            true;
        _ ->
            MaxFreq =
                lists:max(
                    maps:values(Map)),
            case {cave_size(Node), MaxFreq, maps:get(Node, Map, 0)} of
                {small, MF, F} when MF >= 2 andalso F >= 1 ->
                    false;
                _ ->
                    true
            end
    end.

btoa(B) when is_binary(B) ->
    list_to_atom(binary_to_list(B)).

%% Tests

-ifdef(TEST).

ex1_test() ->
    Binary = <<"start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end\n">>,
    ?assertEqual(10, solve1(parse(Binary))),
    ?assertEqual(36, solve2(parse(Binary))).

% freq_test() ->
%     ?assertEqual(none, freq([start, 'A', 'end'])),
%     ?assertEqual({3, a}, freq([a, b, a, c, a, b, d, e])),
%     ?assertEqual({3, a}, freq([start, a, 'A', b, a, c, a, b, d, e])).

-endif.
