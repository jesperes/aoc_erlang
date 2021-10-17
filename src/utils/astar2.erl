%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% Attempt at a slightly simpler (less general) implementation of the
%%% A* search algorithm.
%%%
%%% @end
%%% Created :  4 Jan 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(astar2).
-export([astar/5]).

-type search_node() :: term().
-type cost_fun() :: fun((search_node()) -> integer()).
-type nbr_fun() :: fun((search_node()) -> list(search_node())).
-type dist_fun() :: fun((search_node(), search_node()) -> integer()).
-type end_fun() :: fun((search_node()) -> boolean()).

-type astar_result() ::
        {Distance :: integer(),
         Path :: list(search_node())}
      | 'search_exhausted'.

-spec astar(Start :: search_node(),
            End :: search_node(),
            CostFn :: cost_fun(),
            NbrFn :: nbr_fun(),
            DistFn :: dist_fun()) -> astar_result().
astar(Start, End, CostFn, NbrFn, DistFn) ->
  case is_function(End) of
    true ->
      astar0(Start, CostFn, NbrFn, DistFn, End);
    false ->
      astar0(Start, CostFn, NbrFn, DistFn,
             fun(E) -> E == End end)
  end.

-spec astar0(Start :: search_node(),
             CostFn :: cost_fun(),
             NbrFn :: nbr_fun(),
             DistFn :: dist_fun(),
             EndFn :: end_fun()) -> astar_result().
astar0(Start, CostFn, NbrFn, DistFn, EndFn) ->
  OC = #{Start => open},
  CF = maps:new(),		    %% CameFrom
  Gs = #{Start => 0},
  Fs = gb_sets:singleton({CostFn(Start), Start}),
  astar0(OC, CF, Gs, Fs, CostFn, NbrFn, DistFn, EndFn).

-spec astar0(OC :: map(),
             CF :: map(),
             Gs :: map(),
             Fs :: gb_sets:set(),
             CostFn :: cost_fun(),
             NbrFn :: nbr_fun(),
             DistFn :: dist_fun(),
             EndFn :: end_fun()) -> astar_result().
astar0(OC, CF, Gs, Fs, CostFn, NbrFn, DistFn, EndFn) ->
  case gb_sets:size(Fs) of
    0 ->
      search_exhausted;
    _ ->
      %% erlang:display({fs, gb_sets:to_list(Fs)}),
      {{_, Curr}, Fs0} = gb_sets:take_smallest(Fs),
      case EndFn(Curr) of
        true ->
          Dist = maps:get(Curr, Gs),
          {Dist, path_recon(Curr, CF)};
        false ->
          OC0 = OC#{Curr => closed},
          Nbrs = NbrFn(Curr),

          {_, OC1, CF0, Gs0, Fs1, _, _} =
            lists:foldl(
              fun astar_nbr/2,
              {Curr, OC0, CF, Gs, Fs0, CostFn, DistFn}, Nbrs),
          astar0(OC1, CF0, Gs0, Fs1, CostFn, NbrFn, DistFn, EndFn)
      end
  end.

%% State to keep when folding over the list of neighbors.
-type nbr_fold_state() ::
        { Curr :: search_node(),
          OC :: map(),
          CF :: map(),
          Gs :: map(),
          Fs :: gb_sets:set(),
          CostFn :: cost_fun(),
          DistFn :: dist_fun()}.

%% Function to fold over the neighbors in the recursive step.
-spec astar_nbr(Nbr :: search_node(),
                AccIn :: nbr_fold_state()) ->
                   nbr_fold_state().
astar_nbr(Nbr, {Curr, OC, CF, Gs, Fs, CostFn, DistFn} = AccIn) ->
  case maps:get(Nbr, OC, open) of
    closed ->
      %% Neighbor is already evaluated.
      AccIn;
    open ->
      %% Add (possibly new) neighbor to open set
      OC0 = OC#{Nbr => open},

      %% Check if this path to the neighbor is better. If so
      %% store it and continue.
      Dist = DistFn(Curr, Nbr),
      NewGs = maps:get(Curr, Gs) + Dist,
      OldGs = maps:get(Nbr, Gs, inf),
      if NewGs < OldGs ->
          Cost = CostFn(Nbr),

          %% Fs1 = gb_sets:filter(
          %% 	    fun({_, N}) when N == Nbr -> false;
          %% 	       (_) -> true
          %% 	    end, Fs),

          %% Record new path if better
          {Curr, OC0,
           maps:put(Nbr, Curr, CF),  %% update came-from map
           maps:put(Nbr, NewGs, Gs), %% update neighbor's gscore
           gb_sets:add({NewGs + Cost, Nbr}, Fs),
           CostFn,
           DistFn
          };
         true -> AccIn
      end
  end.

%%% Helper functions
-spec path_recon(Curr :: search_node(),
                 CF :: map()) -> list(search_node()).
path_recon(Curr, CF) ->
  case maps:is_key(Curr, CF) of
    true ->  [Curr|path_recon(maps:get(Curr, CF), CF)];
    false -> [Curr]
  end.
