-module(bfs).

-export([ bfs/3
        , shortest_path/2
        ]).

-record(state,
        { source
        , graph
        , discovered
        , queue
        , parents
        , callbackfun
        }).

shortest_path(State, Node) ->
  shortest_path(State, Node, [Node]).

shortest_path(#state{parents = Parents} = State, Node, Path) ->
  case maps:get(Node, Parents, undef) of
    undef -> Path;
    Parent -> shortest_path(State, Parent, [Parent|Path])
  end.

bfs(Graph, Source, CallbackFun) ->
  State =
    #state{ source = Source
          , graph = Graph
          , discovered = sets:from_list([Source])
          , queue = queue:from_list([Source])
          , parents = #{}
          , callbackfun = CallbackFun
          },

  bfs0(State).

bfs0(#state{ queue = Queue
           , callbackfun = CallbackFun
           , graph = Graph
           } = State) ->

  case queue:out(Queue) of
    {empty, _} -> {finished, State};
    {{value, Node}, Q0} ->
      State0 = State#state{queue = Q0},

      case CallbackFun(Node, Graph) of
        found -> {found, Node, State0};
        Neighbors ->

          State1 =
            lists:foldl(
              fun(Nbr, #state{ queue = Q1
                             , parents = P1
                             , discovered = D1
                             } = Acc) ->
                  case sets:is_element(Nbr, D1) of
                    true -> Acc;
                    false ->
                      Acc#state{ queue = queue:in(Nbr, Q1)
                               , discovered = sets:add_element(Nbr, D1)
                               , parents = maps:put(Nbr, Node, P1)
                               }
                  end
              end, State0, Neighbors),

          bfs0(State1)
      end
  end.
