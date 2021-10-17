-module(aoc2017_day07).

-include_lib("eunit/include/eunit.hrl").

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 7,
                name = "Recursive Circus",
                expected = {xegshds, 299},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: digraph:graph().
-type result_type() :: {atom(), integer()}.

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    Lines = string:tokens(binary_to_list(Binary), "\n\r"),
    List =
        lists:map(fun(Line) ->
                     [Node, Weight | Children] = string:tokens(Line, " ()->,"),
                     {list_to_atom(Node),
                      list_to_integer(Weight),
                      lists:map(fun list_to_atom/1, Children)}
                  end,
                  Lines),
    make_digraph(List).

-spec solve(Input :: input_type()) -> result_type().
solve(Graph) ->
    {yes, Root} = digraph_utils:arborescence_root(Graph),
    {unbalanced, Unbalanced} = find_unbalanced_node(Graph, Root),
    digraph:delete(Graph),
    {Root, Unbalanced}.

%% Find the unbalanced node. Returns the total subtree weight, or
%% {unbalanced, CorrectWeight} if the subtree is unbalanced.
-spec find_unbalanced_node(digraph:graph(), Node :: atom()) ->
                              TotalWeight :: integer() | {unbalanced, CorrectWeight :: integer()}.
find_unbalanced_node(G, Node) ->
    SubTreeWeights =
        lists:foldl(fun (_Child, {unbalanced, _} = Acc) ->
                            Acc;
                        (Child, Acc) ->
                            case find_unbalanced_node(G, Child) of
                                {unbalanced, _} = Result ->
                                    Result;
                                Weight ->
                                    [{Child, Weight} | Acc]
                            end
                    end,
                    [],
                    digraph:out_neighbours(G, Node)),

    {_, Weight} = digraph:vertex(G, Node),

    case SubTreeWeights of
        [] ->
            Weight; % Leaf node
        {unbalanced, _} = Result ->
            Result; % The final result has been found, pass it on.
        _ ->
            %% Check if the subtrees are balanced.
            {Children, Weights} = lists:unzip(SubTreeWeights),
            case deviant(Weights) of
                undef ->
                    Weight + lists:sum(Weights);
                {Deviant, NonDeviant} ->
                    RevMap =
                        maps:from_list(
                            lists:zip(Weights, Children)),
                    {_, DevianWeight} = digraph:vertex(G, maps:get(Deviant, RevMap)),
                    {unbalanced, DevianWeight - (Deviant - NonDeviant)}
            end
    end.

%% Find the one value which deviates from the others in a list.
%% Build a frequency map and select the value which occurs only
%% once. Return 'undef' if the list only contains two values or less,
%% or if there are more than two distinct values.
-spec deviant([integer()]) -> undef | {Deviant :: integer(), NonDeviant :: integer()}.
deviant(List) ->
    case maps:to_list(
             lists:foldl(fun(X, Acc) -> maps:update_with(X, fun(Old) -> Old + 1 end, 1, Acc) end,
                         #{},
                         List))
    of
        [{X, 1}, {Y, M}] when M >= 2 ->
            {X, Y};
        [{Y, M}, {X, 1}] when M >= 2 ->
            {X, Y};
        _ ->
            undef
    end.

%% Construct a digraph from the tuples in the input.
make_digraph(List) ->
    Graph = digraph:new([acyclic]),
    lists:foreach(fun({Node, _Weight, Children}) ->
                     Parent = digraph:add_vertex(Graph, Node),
                     lists:foreach(fun(Child) ->
                                      digraph:add_edge(Graph,
                                                       Parent,
                                                       digraph:add_vertex(Graph, Child))
                                   end,
                                   Children)
                  end,
                  List),
    %% Annotate each vertex with the weight
    lists:foreach(fun({Node, Weight, _}) -> digraph:add_vertex(Graph, Node, Weight) end,
                  List),
    Graph.
