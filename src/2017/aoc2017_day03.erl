-module(aoc2017_day03).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 3,
                name = "Spiral Memory",
                expected = {326, 363010},
                has_input_file = false}.

-type input_type() :: integer().
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    361527.

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    spiral1(Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    spiral2(Input).

%% Part 1

initial_direction() ->
    {0, -1}.

rotate_clockwise({1, 0}) -> % East -> South
    {0, 1};
rotate_clockwise({0, 1}) -> % South -> West
    {-1, 0};
rotate_clockwise({-1, 0}) -> % West -> North
    {0, -1};
rotate_clockwise({0, -1}) -> % North -> East
    {1, 0}.

spiralmemory1_add(N, Coord, {Set, Map}) ->
    {sets:add_element(Coord, Set), maps:put(N, Coord, Map)}.

spiralmemory1_get_coords_at_index(I, {_, Map}) ->
    maps:get(I, Map).

spiralmemory1_has_coord(Coord, {Set, _}) ->
    sets:is_element(Coord, Set).

spiralmemory1_new() ->
    {sets:new(), maps:new()}.

%%% ------------------------------------------------------------

get_next_coord_rotated({X, Y}, Delta) ->
    NewDelta = {XD, YD} = rotate_clockwise(Delta),
    NewCoord = {X + XD, Y + YD},
    {NewCoord, NewDelta}.

get_next_coord_forward({X, Y}, Delta) ->
    {XD, YD} = Delta,
    NewCoord = {X + XD, Y + YD},
    {NewCoord, Delta}.

%%%
%%% Compute the location of the Nth spiral element, and return
%%% its manhattan distance from the center.
spiral1(N) when N >= 1 ->
    Init = 1,
    Elems = spiral1(Init, N, spiralmemory1_new(), initial_direction()),
    {X, Y} = spiralmemory1_get_coords_at_index(N, Elems),
    abs(X) + abs(Y).

%%% spiral/4: collect a map of spiral elements starting at 1 and
%%% ending at N.
spiral1(1, N, Map, Delta) ->
    %% Starting coordinates is {0, 0}
    spiral1(2, N, spiralmemory1_add(1, {0, 0}, Map), Delta);
spiral1(I, N, Map, _) when I > N ->
    Map;
spiral1(I, N, Map, Delta) ->
    %% Coordinate of the previous cell
    Coord = spiralmemory1_get_coords_at_index(I - 1, Map),

    %% The new coordinate + delta if we rotate
    {NewCoordRotated, NewDeltaRotated} = get_next_coord_rotated(Coord, Delta),

    %% New coordinate + delta if we go straight. Note that
    %% the delta is same as before.
    {NewCoordStraight, Delta} = get_next_coord_forward(Coord, Delta),

    %% TODO this is inefficient since we recompute the list of map
    %% values for each iteration.
    CanRotate = not spiralmemory1_has_coord(NewCoordRotated, Map),

    %% Compute the coordinates of the new cell, and the new delta.
    {ActualNewCoord, ActualNewDelta} =
        if CanRotate ->
               %% If the cell at the rotated position is already occupied,
               %% continue in the same direction as before.
               {NewCoordRotated, NewDeltaRotated};
           true ->
               %% Otherwise, continue in the direction of the rotated
               %% coordinates
               {NewCoordStraight, Delta}
        end,

    spiral1(I + 1, N, spiralmemory1_add(I, ActualNewCoord, Map), ActualNewDelta).

%% Part 2

spiralmemory2_add(N, Value, Coord, {Set, Map, MapC}) when is_integer(N) and
							 is_integer(Value) and
							 is_tuple(Coord)
							 ->
  {
   %% The set contains all coordinates so that we quickly can
   %% lookup if a cell is used or not.
   sets:add_element(Coord, Set),

   %% The maps uses the index as key, and maps to the value and the
   %% coordinate.
   maps:put(N, {Value, Coord}, Map),

   %% Maps coordinate to its value
   maps:put(Coord, Value, MapC)
  }.

spiralmemory2_get_element_at_index(I, {_, Map, _}) ->
  maps:get(I, Map).

spiralmemory2_get_value_at_coord(Coord, {_, _, MapC}) ->
  maps:get(Coord, MapC, 0).

spiralmemory2_has_coord(Coord, {Set, _, _}) ->
  sets:is_element(Coord, Set).

spiralmemory2_new() ->
  {sets:new(), maps:new(), maps:new()}.

spiralmemory2_get_new_value_at({X,Y}, SpiralMemory) ->
  {_List, Value} =
    lists:mapfoldr(
      fun({Dx,Dy}, AccIn) ->
          DCoord = {X + Dx, Y + Dy},
          case spiralmemory2_get_value_at_coord(DCoord, SpiralMemory) of
            {badmap, _} ->
              {0, AccIn};
            Value ->
              {Value, Value + AccIn}
              %% {{DCoord,{d,Dx,Dy},Value}, Value + AccIn}
          end
      end, 0, [{Dx,Dy} || Dx <- [-1,0,1], Dy <- [-1,0,1]]),
  Value.

spiralmemory2_get_latest_value(SM = {_, Map, _}) ->
  LargestIndex = lists:max(maps:keys(Map)),
  {Value, _} = spiralmemory2_get_element_at_index(LargestIndex, SM),
  Value.

%%%
%%% Compute the location of the Nth spiral element, and return
%%% its manhattan distance from the center.
spiral2(N) when N >= 1 ->
  Init = 1,
  SM = spiral2(Init, N, spiralmemory2_new(), initial_direction()),
  spiralmemory2_get_latest_value(SM).

%%% spiral/4: collect a map of spiral elements starting at 1 and
%%% ending at N.
spiral2(1, N, SpiralMemory, Delta) ->
  %% Starting coordinates is {0, 0}
  spiral2(2, N, spiralmemory2_add(1, 1, {0, 0}, SpiralMemory), Delta);
spiral2(I, N, SpiralMemory, _) when I > N ->
  SpiralMemory;
spiral2(I, N, SpiralMemory, Delta) ->
  %% io:format("Step ~w~n", [I]),

  %% Coordinate of the previous cell
  {_, Coord} = spiralmemory2_get_element_at_index(I - 1, SpiralMemory),

  %% The new coordinate + delta if we rotate
  {NewCoordRotated, NewDeltaRotated} = get_next_coord_rotated(Coord, Delta),

  %% New coordinate + delta if we go straight. Note that
  %% the delta is same as before.
  {NewCoordStraight, Delta} = get_next_coord_forward(Coord, Delta),

  CanRotate = not spiralmemory2_has_coord(NewCoordRotated, SpiralMemory),

  %% Compute the coordinates of the new cell, and the new delta.
  {ActualNewCoord, ActualNewDelta} =
    if CanRotate ->
        %% If the cell at the rotated position is already occupied,
        %% continue in the same direction as before.
        {NewCoordRotated, NewDeltaRotated};
       true ->
        %% Otherwise, continue in the direction of the rotated
        %% coordinates
        {NewCoordStraight, Delta}
    end,

  NewValue = spiralmemory2_get_new_value_at(ActualNewCoord, SpiralMemory),
  NewSpiralMemory = spiralmemory2_add(I, NewValue, ActualNewCoord, SpiralMemory),

  if NewValue > N ->
                                                % io:format("NewValue ~w > N ~w~n, exiting", [NewValue, N]),
      NewSpiralMemory;
     true ->
      spiral2(I + 1, N, NewSpiralMemory, ActualNewDelta)
  end.
