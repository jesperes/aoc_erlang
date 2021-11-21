-module(aoc2016_day11).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

% -compile([export_all, nowarn_export_all]).

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

%% Represent the current state using a binary:
%%
%% <<EF, RTG1, MC1, RTG2, MC2, ... >>
%%
%% EF   - the floor the elevator is on
%% RTFn - the floor RTG <n> is on
%% MCn  - the floor microchip <n> is on
%%
-type state() :: binary().
-type input_type() :: state().
-type result_type() :: integer().

-define(NUM_ELEMENTS(Obj), byte_size(Obj) div 2).

-spec parse(Binary :: binary()) -> input_type().
parse(_Binary) ->
    <<1,      %% elevator floor
      1, 1,   %% strontium
      1, 1,   %% plutonium
      2, 3,   %% thulium
      2, 2,   %% ruthenium
      2, 2>>. %% curium

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    find_path(Input).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    Extras = <<1, 1, 1, 1>>,
    find_path(<<Input/binary, Extras/binary>>).

find_path(Start) ->
    %% GScore is the cost of the cheapest path from start to n
    %% currently known.
    GScore = #{Start => 0},

    H = heuristic(Start),

    %% FScore is the current best guess as to how short a path from
    %% start to finish can be if it goes through n.
    FScore = #{Start => H},

    OpenSet = gb_sets:from_list([{H, Start}]),

    Goal = list_to_binary(lists:duplicate(byte_size(Start), 4)),

    find_path(GScore, FScore, OpenSet, Goal).

find_path(G, F, OpenSet, Goal) ->
    {{Dist, Node}, O0} = gb_sets:take_smallest(OpenSet),
    F0 = maps:remove(Node, F),

    case Node =:= Goal of
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

            find_path(NewG, NewF, NewO, Goal)
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
-spec heuristic(binary()) -> number().
heuristic(<<_EF, Objects/binary>>) ->
    heuristic0(Objects).

heuristic0(<<>>) -> 0;
heuristic0(<<RTG, MC, Rest/binary>>) ->
    (4 - RTG) + (4 - MC) + heuristic0(Rest).

%% Neighbors
neighbors(<<EF, Objects/binary>> = _S) ->
    %% ?debugFmt("State = ~p", [S]),

    Seq = lists:seq(0, ?NUM_ELEMENTS(Objects) - 1),
    Floors = [max(1, EF - 1), min(4, EF + 1)],
    Types = [0, 1],

    States =
        %% 1-element moves
        [State ||
            DestFloor <- Floors,
            DestFloor =/= EF,
            Idx <- Seq,
            Type <- Types,
            binary:at(Objects, Idx * 2 + Type) == EF, %% remove objects not on the elevator's floor
            State <- [apply_move(DestFloor, Type, Idx, Objects)],
            is_valid_state(State)] ++

        %% 2-element moves
        [State ||
            DestFloor <- Floors,
            DestFloor =/= EF,
            Idx1 <- Seq,
            Idx2 <- Seq,
            Type1 <- Types,
            Type2 <- Types,
            {Type1, Idx1} < {Type2, Idx2},
            binary:at(Objects, Idx1 * 2 + Type1) == EF,
            binary:at(Objects, Idx2 * 2 + Type2) == EF,
            State <- [apply_move(DestFloor, Type1, Idx1, Type2, Idx2, Objects)],
            is_valid_state(State)],

    States0 = lists:map(fun(State) ->
                                sort_state(State)
                        end, States),

    lists:usort(States0).


%% Sort each state to prune equivalent states. Despite converting
%% to/from a list, this is a very important step since it keeps down
%% the size of the search tree.
sort_state(<<EF, Objects/binary>>) ->
    <<EF, (from_tuples(lists:sort(to_tuples(Objects))))/binary>>.

to_tuples(<<>>) ->
    [];
to_tuples(<<X, Y, Rest/binary>>) ->
    [{X, Y}|to_tuples(Rest)].

from_tuples([]) ->
    <<>>;
from_tuples([{X, Y}|Rest]) ->
    <<X, Y, (from_tuples(Rest))/binary>>.

apply_move(DestFloor, Type, Idx, Objects) ->
    <<DestFloor, (replace_byte_at(Objects, Idx * 2 + Type, DestFloor))/binary>>.

apply_move(DestFloor, Type1, Idx1, Type2, Idx2, Objects) ->
    Objects0 = replace_byte_at(Objects, Idx1 * 2 + Type1, DestFloor),
    Objects1 = replace_byte_at(Objects0, Idx2 * 2 + Type2, DestFloor),
    <<DestFloor, Objects1/binary>>.

is_valid_state(<<_EF, Objects/binary>>) ->
    check_elements(Objects, Objects, 0).

check_elements(_, Objects, Idx) when Idx >= ?NUM_ELEMENTS(Objects) ->
    true;
check_elements(<<FloorRTG, FloorMC, Rest/binary>>, Objects, Idx) when FloorRTG == FloorMC ->
    %% Chip is shielded
    check_elements(Rest, Objects, Idx + 1);
check_elements(<<_, FloorMC, Rest/binary>>, Objects, Idx) ->
    %% Check that there are no other RTGs on this floor.
    case check_no_rtgs(FloorMC, Idx, Objects, 0) of
        true ->
            check_elements(Rest, Objects, Idx + 1);
        false ->
            false
    end.

check_no_rtgs(_, _SkipIdx, Objects, Idx) when Idx >= byte_size(Objects) ->
    true;
check_no_rtgs(FloorMC, SkipIdx, <<_, _, Rest/binary>>, Idx) when Idx == SkipIdx ->
    check_no_rtgs(FloorMC, SkipIdx, Rest, Idx + 1);
check_no_rtgs(FloorMC, _SkipIdx, <<FloorRTG, _, _Rest/binary>>, _Idx) when FloorMC == FloorRTG ->
    false;
check_no_rtgs(FloorMC, SkipIdx, <<_, _, Rest/binary>>, Idx) ->
    check_no_rtgs(FloorMC, SkipIdx, Rest, Idx + 1).

replace_byte_at(Binary, Idx, Replace) when Idx < byte_size(Binary) ->
    X = binary:part(Binary, {0, Idx}),
    Y = binary:part(Binary, {Idx + 1, byte_size(Binary) - Idx - 1}),
    Out = <<X/binary, Replace, Y/binary>>,
    %% ?assertEqual(byte_size(Binary), byte_size(Out)),
    %% ?debugFmt("~p -> replacing idx ~p with ~p -> ~p",
    %%           [Binary, Idx, Replace, Out]),
    Out.

replace_byte_at_test() ->
    ?assertEqual(<<1, 2, 42, 4, 5, 6>>, replace_byte_at(<<1, 2, 3, 4, 5, 6>>, 2, 42)).

ex1_test() ->
    ?assertEqual(11, find_path(<<1, 2, 1, 3, 1>>)).
