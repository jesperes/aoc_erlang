-module(aoc2021_day23).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-compile([export_all, nowarn_export_all]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 23,
                name = "Amphipod",
                expected = {0, 0},
                has_input_file = true}.

-type input_type() :: any().
-type result_type() :: integer().

offset_to_coord(Offset, Width) ->
    {Offset rem (Width + 1), Offset div (Width + 1)}.

goal() ->
    #{{3, 2} => $A,
      {3, 3} => $A,
      {5, 2} => $B,
      {5, 3} => $B,
      {7, 2} => $C,
      {7, 3} => $C,
      {9, 2} => $D,
      {9, 3} => $D}.

% Note that this requires slightly modified input file, since the downloaded
% input is not strictly rectangular.
coord_map(Binary, Chars) ->
    {Width, _} = binary:match(Binary, <<"\n">>),
    List =
        lists:zip(
            lists:seq(0, size(Binary) - 1), binary_to_list(Binary)),
    lists:foldl(fun({Offset, C0}, Acc) ->
                   case lists:member(C0, Chars) of
                       true -> maps:put(offset_to_coord(Offset, Width), C0, Acc);
                       false -> Acc
                   end
                end,
                #{},
                List).

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    Walls = coord_map(Binary, "#"),
    Amphipods = coord_map(Binary, "ABCD"),
    io:format(standard_error,
              "~s~n",
              [grid:to_str(
                   maps:merge(Amphipods, Walls))]),
    {Walls, Amphipods}.

-spec solve1(Input :: input_type()) -> result_type().
solve1({Walls, Amphipods}) ->
    Explored = sets:new(),
    Frontier = gb_sets:from_list([{0, Amphipods}]),
    find_shortest_path(Explored, Frontier, Walls),
    0.

-spec solve2(Input :: input_type()) -> result_type().
solve2(_Input) ->
    0.

% 17478

%% Dijkstra
find_shortest_path(Explored, Frontier, Walls) ->
    {{Cost, Amphipods} = Node, Frontier0} = gb_sets:take_smallest(Frontier),
    case Amphipods =:= goal() of
        true ->
            Cost;
        false ->
            Explored0 = sets:add_element(Amphipods, Explored),
            lists:foldl(fun(Nbr, FrontierIn) ->
                           ?_if(sets:is_element(Nbr, Explored0),
                                FrontierIn,
                                gb_sets:add(Nbr, FrontierIn))
                        end,
                        Frontier0,
                        neighbors(Node, Walls))
    end.

neighbors({_Cost, Amphipods}, _Walls) ->
    % Cost = the cost/distance from the start to this point
    % Amphipods = the current positions of the amphipods (Coord => Char map)
    % Walls = the position of the walls
    %
    % At each step:
    % Every amphipod can make one of two moves:
    % 1. Move from its room into a spot in the hallway
    % 2. Move from the hallway into its final destination
    Nbrs =
        lists:foldl(fun(Amphipod, Acc) -> nbr_fold_fun(Amphipod, Amphipods, Acc) end,
                    [],
                    maps:to_list(Amphipods)),
    throw(Nbrs).

nbr_fold_fun({{_, Y} = Coord, Type}, Amphipods, Acc) when Y =:= 1 ->
    case move_to_dest(Coord, Type, Amphipods) of
        false ->
            Acc;
        DestCoord ->
            [{cost(Coord, DestCoord, Type), Coord} | Acc]
    end;
nbr_fold_fun({{_X, Y} = Coord, Type}, Amphipods, Acc) when Y >= 2 ->
    lists:map(fun(HallwayCoord) -> {cost(Coord, HallwayCoord, Type), HallwayCoord} end,
              free_hallway_positions(Coord, Amphipods))
    ++ Acc.

move_to_dest(Coord, Type, Amphipods) ->
    false.

cost({X0, Y0}, {X1, Y1}, Type) ->
    cost(Type) * (abs(X1 - X0) + abs(Y1 - Y0)).

% Find the valid hallway positions for an amphipod
free_hallway_positions({StartX, StartY}, Amphipods) ->
    % First, check if the amphipod can move at all; e.g. if it has another
    % amphipod directly above it (or it is at directly below the hallway).
    ?_if(StartY > 2 andalso maps:is_key({StartX, StartY - 1}, Amphipods),
         [],
         is_free(StartX - 1, -1, Amphipods) ++ is_free(StartX + 1, 1, Amphipods)).

is_free(X, Dx, Amphipods) when X =:= 3 orelse X =:= 5 orelse X =:= 7 orelse X =:= 9 ->
    % Never stop outside any room
    is_free(X + Dx, Dx, Amphipods);
is_free(X, Dx, Amphipods) when X >= 1 andalso X =< 11 ->
    ?_if(maps:is_key({X, 1}, Amphipods), [], [{X, 1} | is_free(X + Dx, Dx, Amphipods)]);
is_free(_, _, _) ->
    [].

% Is the destination room for the given amphipod type "free", i.e. empty or only
% contains amphipods of the same type.
is_dest_free(Type, Amphipods) ->
    FinalX = final_dest(Type),
    maps:filter(fun ({X, _Y}, T) when X =:= FinalX andalso T =/= Type ->
                        true;
                    (_, _) ->
                        false
                end,
                Amphipods)
    =:= [].

final_dest($A) ->
    3;
final_dest($B) ->
    5;
final_dest($C) ->
    7;
final_dest($D) ->
    9.

cost($A) ->
    1;
cost($B) ->
    10;
cost($C) ->
    100;
cost($D) ->
    1000.

-ifdef(TEST).

%% Tests

%% ...

-endif.
