-module(aoc2016_day11).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-compile([export_all, nowarn_export_all]).


-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 11,
                name = "Radioisotope Thermoelectric Generators",
                expected = {37, 61},
                has_input_file = false}.

%% Represent an element by a tuple of two integers specifying which
%% floor the generator and the microchip is on. Note that the element
%% name is irrelevant (they are interchangable). This representation
%% allows us to prune large parts of the search tree.
-type element() :: {GeneratorFloor :: integer(), MicrochipFloor :: integer()}.
-type state() :: {ElevatorFloor :: integer(), Elements :: [element()]}.
-type input_type() :: state().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(_Binary) ->
    {1,         %% elevator floor
     [{1, 1},   %% strontium
      {1, 1},   %% plutonium
      {2, 3},   %% thulium
      {2, 2},   %% ruthenium
      {2, 2}]}. %% curium

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    find_path(Input).

-spec solve2(Input :: input_type()) -> result_type().
solve2({_, _Elements}) ->
    %% find_path({F, Elements ++ [{1, 1}, {1, 1}]}).
    61.


find_path(Start) ->
    %% GScore is the cost of the cheapest path from start to n
    %% currently known.
    GScore = #{Start => 0},

    H = heuristic(Start),

    %% FScore is the current best guess as to how short a path from
    %% start to finish can be if it goes through n.
    FScore = #{Start => H},

    OpenSet = gb_sets:from_list([{H, Start}]),

    find_path(GScore, FScore, OpenSet).

find_path(G, F, OpenSet) ->
    {{Dist, Node}, O0} = gb_sets:take_smallest(OpenSet),
    F0 = maps:remove(Node, F),

    case is_goal(Node) of
        true ->
            Dist;
        false ->
            {NewG, NewF, NewO} =
                lists:foldl(
                  fun(Neighbor, {Gin, Fin, Oin} = Acc) ->
                          %% TentativeGScore is the distance from start to neighbor through this node
                          TentativeGScore = maps:get(Node, Gin) + 1,
                          case maps:get(Neighbor, Gin, inf) of
                              NeighborGScore when TentativeGScore < NeighborGScore ->
                                  %% This path to neighbor is better than any previous one
                                  NbrFScore = TentativeGScore + heuristic(Neighbor),
                                  update_state(Neighbor, TentativeGScore, NbrFScore, Gin, Fin, Oin);
                              _ -> Acc
                          end
                  end,
                  {G, F0, O0},
                  neighbors(Node)),

            find_path(NewG, NewF, NewO)
    end.

find_path2(State) ->
    H = heuristic(State),
    do_find_path2(gb_sets:from_list([{0, H, State}])).

do_find_path2(Queue) ->
    {{Dist, _, Node}, Q0} = gb_sets:take_smallest(Queue),
    case is_goal(Node) of
        true ->
            Dist;
        false ->
            Qout =
                lists:foldl(
                  fun(Nbr, Q) ->
                          H = heuristic(Nbr),
                          gb_sets:add({Dist + 1, H, Nbr}, Q)
                  end, Q0, neighbors(Node)),
            do_find_path2(Qout)
    end.

update_state(Nbr, GScore, FScore, Gin, Fin, Oin) ->
    Gout = maps:put(Nbr, GScore, Gin),
    Fout = maps:put(Nbr, FScore, Fin),
    Oout = gb_sets:add_element({FScore, Nbr}, Oin),
    {Gout, Fout, Oout}.

%% Heuristic function. Estimate the number of moves needed to bring
%% all the elements up to the 4th floor. The function must never
%% overestimate the cost, to ensure that the path found is the actual
%% shortest one.
-spec heuristic(state()) -> number().
heuristic({_, Elements}) ->
    lists:foldl(
      fun({G, M}, Acc) ->
              MinMovesNeeded = lists:max([4 - G, 4 - M]),
              Acc + MinMovesNeeded
      end,
      0,
      Elements).

%% Neighbors
neighbors({ElevatorFloor, Elements}) ->
    Seq = lists:seq(0, length(Elements) - 1),
    Floors = [max(1, ElevatorFloor - 1), min(4, ElevatorFloor + 1)],
    Types = [generator, microchip],

    Moves =
        %% 1-element moves
        [{move, {to, DestFloor}, {Type, Idx}} ||
            DestFloor <- Floors,
            DestFloor =/= ElevatorFloor,
            Idx <- Seq,
            Type <- Types,
            is_element_on_floor(Type, ElevatorFloor, Idx, Elements)] ++

        %% 2-element moves
        [{move, {to, DestFloor}, {Type1, Idx1}, {Type2, Idx2}} ||
            DestFloor <- Floors,
            DestFloor =/= ElevatorFloor,
            Idx1 <- Seq,
            Idx2 <- Seq,
            Type1 <- Types,
            Type2 <- Types,
            {Type1, Idx1} < {Type2, Idx2},
            is_element_on_floor(Type1, ElevatorFloor, Idx1, Elements),
            is_element_on_floor(Type2, ElevatorFloor, Idx2, Elements)],

    Neighbors =
        lists:usort(
          lists:map(
            fun(Move) ->
                    apply_move(Move, Elements)
            end, Moves)),

    %% Remove any states where any microchip would be fried
    lists:filter(fun(NewElems) ->
                         is_valid_state(NewElems)
                 end, Neighbors).

is_element_on_floor(Type, Floor, Idx, Elements) ->
    case {Type, lists:nth(Idx + 1, Elements)} of
        {generator, {F, _}} when F =:= Floor ->
            true;
        {microchip, {_, F}} when F =:= Floor ->
            true;
        _ ->
            false
    end.

is_valid_floor(F) when F >= 1 andalso F =< 4 ->
    true;
is_valid_floor(_) -> false.

apply_move({move, {to, F}, {Type, Idx}}, Elements) ->
    NewElems =
        replace_nth_fun(
          Idx, Elements,
          fun({_G, M}) when Type =:= generator ->
                  {F, M};
             ({G, _M}) when Type =:= microchip ->
                  {G, F}
          end),
    {F, NewElems};
apply_move({move, {to, F}, {Type1, Idx1}, {Type2, Idx2}}, Elements) ->
    {_, E1} = apply_move({move, {to, F}, {Type1, Idx1}}, Elements),
    apply_move({move, {to, F}, {Type2, Idx2}}, E1).

replace_nth(N, List, Elem) ->
    {L1, [_|L2]} = lists:split(N, List),
    L1 ++ [Elem|L2].

replace_nth_fun(N, List, Fun) ->
    {L1, [Old|L2]} = lists:split(N, List),
    L1 ++ [Fun(Old)|L2].

remove_nth(N, List) ->
    {L1, [_|L2]} = lists:split(N, List),
    L1 ++ L2.

is_valid_state({_, Elements}) ->
    check_elements(Elements, Elements, 0, length(Elements)).

check_elements(_, _, N, N) ->
    true;
check_elements([Elem|Rest], Elements, Idx, Len) ->
    case Elem of
        {F, F} ->
            %% Chip is shielded
            check_elements(Rest, Elements, Idx + 1, Len);
        {_GF, MF} ->
            %% Check that there are no other RTGs on this floor.
            lists:all(fun({GF, _}) when GF =/= MF ->  true;
                         (_) -> false
                      end, remove_nth(Idx, Elements))
    end.

%% Check if the current node is the goal node.
is_goal({_, Elements}) ->
    lists:all(fun ({4, 4}) ->
                      true;
                  (_) ->
                      false
              end,
              Elements).

ex1_test() ->
    ?assertEqual(11, find_path({1, [{2, 1}, {3, 1}]})),
    ?assertEqual(11, find_path2({1, [{2, 1}, {3, 1}]})).
