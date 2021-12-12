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
                expected = {4549, -1},
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
                     digraph:add_edge(G, A, B),
                     digraph:add_edge(G, B, A)
                  end,
                  Edges),
    G.

-spec solve1(Input :: input_type()) -> result_type().
solve1(G) ->
    length(find_all_paths(G, start, sets:new(), [], [], 0)).

-spec solve2(Input :: input_type()) -> result_type().
solve2(_G) ->
    0.

-if(false).

indented_debugFmt(Depth, Fmt, Args) ->
    Str = lists:duplicate(Depth * 4, " ") ++ io_lib:format(Fmt, Args),
    io:format("~s~n", [Str]).

-else.

indented_debugFmt(_Depth, _Fmt, _Args) ->
    ok.

-endif.

cave_size(Name) ->
    [N | _] = atom_to_list(Name),
    if N >= $A andalso N =< $Z ->
           large;
       true ->
           small
    end.

find_all_paths(G, Node, Visited, CurrentPath, Paths, Depth) ->
    indented_debugFmt(Depth, "visiting ~p", [Node]),
    CurrentPath0 = [Node | CurrentPath],
    % ?debugFmt("Current path: ~p, visited = ~p", [lists:reverse(CurrentPath0), sets:to_list(Visited)]),
    Visited0 = sets:add_element(Node, Visited),
    Nbrs = digraph:out_neighbours(G, Node),
    lists:foldl(fun(Nbr, PathsIn) ->
                   case Nbr of
                       'end' ->
                           P = ['end' | CurrentPath0],
                           indented_debugFmt(Depth + 1,
                                             "visiting end, found path ~p",
                                             [lists:reverse(P)]),
                           [P | PathsIn];
                       _ ->
                           case {cave_size(Nbr), sets:is_element(Nbr, Visited0)} of
                               {small, true} ->
                                   indented_debugFmt(Depth, "not visiting ~p, already seen", [Nbr]),
                                   PathsIn;
                               _ ->
                                   find_all_paths(G,
                                                  Nbr,
                                                  Visited0,
                                                  CurrentPath0,
                                                  PathsIn,
                                                  Depth + 1)
                           end
                   end
                end,
                Paths,
                Nbrs).

btoa(B) when is_binary(B) ->
    list_to_atom(binary_to_list(B)).

%% Tests

-ifdef(TEST).

ex1_test() ->
    Binary = <<"start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end\n">>,
    ?assertEqual(10, solve1(parse(Binary))),
    ?assertEqual(36, solve2(parse(Binary))).

-endif.
