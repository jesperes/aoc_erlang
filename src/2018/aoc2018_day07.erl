-module(aoc2018_day07).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
  #aoc_puzzle{module = ?MODULE,
              year = 2018,
              day = 7,
              name = "The Sum of Its Parts",
              expected = {"BFLNGIRUSJXEHKQPVTYOCZDWMA", 880},
              has_input_file = true}.

-type input_type() :: [{atom(), atom()}].
-type result1_type() :: string().
-type result2_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  Lines = string:tokens(binary_to_list(Input), "\n\r"),
  lists:map(fun(Line) ->
               ["Step",
                Before,
                "must",
                "be",
                "finished",
                "before",
                "step",
                After,
                "can",
                "begin."] =
                 string:tokens(Line, " "),
               {list_to_atom(Before), list_to_atom(After)}
            end,
            Lines).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
  find_step_order(Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
  {Edges, NumWorkers, Duration} = {Input, 5, 60},
  Workers = [{'.', -1} || _ <- lists:seq(1, NumWorkers)],
  find_step_order2(Edges, Workers, Duration).

get_nodes(Edges) ->
  get_nodes(Edges, sets:new()).

get_nodes([], Nodes) ->
  sets:to_list(Nodes);
get_nodes([{In, Out} | Edges], Nodes) ->
  S1 = sets:add_element(In, Nodes),
  S2 = sets:add_element(Out, S1),
  get_nodes(Edges, S2).

get_start_node(Edges) ->
  OutgoingNodes = [{Node, incoming_edges(Edges, Node)} || Node <- get_nodes(Edges)],

  StartNodes =
    lists:filtermap(fun({N, OutEdges}) ->
                       case length(OutEdges) of
                         0 -> {true, N};
                         _ -> false
                       end
                    end,
                    OutgoingNodes),

  [FirstStartNode | _] = lists:sort(StartNodes),
  FirstStartNode.

incoming_edges([], _) ->
  [];
incoming_edges([{Out, Node} | Edges], Node) ->
  [Out | incoming_edges(Edges, Node)];
incoming_edges([_ | Edges], Node) ->
  incoming_edges(Edges, Node).

find_step_order(Edges) ->
  CompletedNodes = [get_start_node(Edges)],
  Remaining = lists:sort(get_nodes(Edges)) -- CompletedNodes,
  List = find_step_order(CompletedNodes, Remaining, Edges),
  lists:flatten(
    lists:map(fun atom_to_list/1, List)).

%% CompletedNodes is the list of nodes already ordered. Remaining is the list
%% of nodes not yet ordered. This is kept sorted alphabetically.
%% Edges is the list of {Out,In} tuples describing the ordering
%% constraints.
find_step_order(CompletedNodes, [], _) ->
  CompletedNodes;
find_step_order(CompletedNodes, Remaining, Edges) ->
  Candidates =
    lists:filter(fun(R) -> is_satisfied(R, CompletedNodes, Edges) end, Remaining),

  [Candidate | _] = Candidates,

  find_step_order(CompletedNodes ++ [Candidate], lists:delete(Candidate, Remaining), Edges).

%% Predicate returning true if Node has all of its incoming edges
%% present in CompletedNodes.
is_satisfied(Node, CompletedNodes, Edges) ->
  lists:subtract(requirements(Node, Edges), CompletedNodes) =:= [].

%% Returns the requirements of Node.
requirements(_Node, []) ->
  [];
requirements(Node, [{Out, Node} | Edges]) ->
  [Out | requirements(Node, Edges)];
requirements(Node, [_ | Edges]) ->
  requirements(Node, Edges).

%% -----------------------------------------------------------------------------
%% Part 2
%% -----------------------------------------------------------------------------

find_step_order2(Edges, Workers, Duration) ->
  find_step_order2(0, [], get_nodes(Edges), Edges, Workers, Duration).

find_step_order2(Second, _CompletedNodes, [], _, _, _) ->
  Second - 1;
find_step_order2(Second, CompletedNodes, Remaining, Edges, Workers, Duration) ->
  %% Second is the current second we are on. CompletedNodes,
  %% Remaining, Edges are as before. Workers is a list of workers
  %% where each element contains a tuple {Task,FinishedAt}
  %% describing which node the worker is working on, and when it is
  %% expected to finish.  Duration is the base-duration of each
  %% step.
  FinishedWork =
    lists:filtermap(fun ({Task, FinishedAt}) when (FinishedAt =< Second) and (Task /= '.') ->
                          %% Do not add completed work more than once
                          case lists:member(Task, CompletedNodes) of
                            true ->
                              false;
                            false ->
                              {true, Task}
                          end;
                        (_) ->
                          false
                    end,
                    Workers),

  NewCompletedNodes = CompletedNodes ++ FinishedWork,

  NewRemaining = lists:subtract(Remaining, NewCompletedNodes),

  %% Compute the list of nodes which have all their prerequisites
  %% fulfilled. Make sure to include nodes which have been completed
  %% in this step.
  Candidates =
    lists:filter(fun(R) ->
                    is_satisfied(R, NewCompletedNodes, Edges) and not is_worked_on(Workers, R)
                 end,
                 Remaining),

  {NewWorkers, _} =
    lists:mapfoldl(fun ({_Task, FinishedAt}, CandIn)
                         when (FinishedAt =< Second) and (length(CandIn) > 0) ->
                         %% We have a idle worker, and we have available steps
                         {[NewTask], CandRest} = lists:split(1, CandIn),
                         {{NewTask, Second + work_duration(NewTask, Duration)}, CandRest};
                       (Busy, CandIn) ->
                         {Busy, CandIn}
                   end,
                   Candidates,
                   Workers),

  find_step_order2(Second + 1,
                   NewCompletedNodes,
                   NewRemaining,
                   Edges,
                   NewWorkers,
                   Duration).

is_worked_on([], _) ->
  false;
is_worked_on([{Task, _} | _], Task) ->
  true;
is_worked_on([_ | Workers], Task) ->
  is_worked_on(Workers, Task).

work_duration(N, BaseDur) ->
  [X] = atom_to_list(N),
  BaseDur + X - $A + 1.
