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
                expected = {13520, 0},
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
    coord_map(Binary, "ABCD").

-spec solve1(Input :: input_type()) -> result_type().
solve1(Amphipods) ->
    find_shortest_path(Amphipods).

-spec solve2(Input :: input_type()) -> result_type().
solve2(_Amphipods) ->
    0.
    % ShiftDown =
    %     lists:foldl(fun ({{X, Y}, Type}, Acc) when Y == 3 ->
    %                         maps:put({X, 5}, Type, Acc);
    %                     ({Coord, Type}, Acc) ->
    %                         maps:put(Coord, Type, Acc)
    %                 end,
    %                 maps:to_list(Amphipods)),
    % NewAmphipods =
    %     #{{3, 3} => $D,
    %       {3, 4} => $D,
    %       {5, 3} => $C,
    %       {5, 4} => $B,
    %       {7, 3} => $B,
    %       {7, 4} => $A,
    %       {9, 3} => $A,
    %       {9, 4} => $C},
    % find_shortest_path(maps:merge(ShiftDown, NewAmphipods)).

find_shortest_path(Amphipods) ->
    Gs = #{Amphipods => 0},
    Fs = gb_sets:singleton({0, Amphipods}),
    find_shortest_path(Gs, Fs).

find_shortest_path(Gs, Fs) ->
    {{Cost, Amphipods}, Fs0} = gb_sets:take_smallest(Fs),
    case Amphipods =:= goal() of
        true ->
            Cost;
        false ->
            {NewGs, NewFs} =
                lists:foldl(fun({NbrCost, NbrAmphipods}, {GsIn, FsIn} = Acc) ->
                               MaybeNewScore = maps:get(Amphipods, GsIn) + NbrCost,
                               case MaybeNewScore < maps:get(NbrAmphipods, GsIn, infinity) of
                                   true ->
                                       BetterScore = MaybeNewScore,
                                       GsOut = maps:put(NbrAmphipods, BetterScore, GsIn),
                                       NewDist =
                                           BetterScore + lower_bound_dist_to_goal(NbrAmphipods),
                                       FsOut = gb_sets:add({NewDist, NbrAmphipods}, FsIn),
                                       {GsOut, FsOut};
                                   false -> Acc
                               end
                            end,
                            {Gs, Fs0},
                            neighbors(Amphipods)),
            find_shortest_path(NewGs, NewFs)
    end.

% Estimate distance to goal by counting steps for all amphipods
lower_bound_dist_to_goal(Amphipods) ->
    maps:fold(fun ({X, Y}, Type, Acc) when Y == 1 ->
                      % for hallway-amphipods the minimum distance is the number
                      % of steps from the hallway into their destination room
                      cost({X, Y}, {final_dest(Type), 2}, Type) + Acc;
                  ({X, _Y} = Coord, Type, Acc) ->
                      % amphipods in rooms may need to move into the hallway
                      % first
                      FinalX = final_dest(Type),
                      ?_if(FinalX == X,
                           0,
                           cost(Coord, {X, 1}, Type)
                           + cost({X, 1}, {FinalX, 2}, Type)) % cost from hallway into room
                      + Acc
              end,
              0,
              Amphipods).

neighbors(Amphipods) ->
    lists:foldl(fun(Amphipod, Acc) -> nbr_fold_fun(Amphipod, Amphipods, Acc) end,
                [],
                maps:to_list(Amphipods)).

nbr_fold_fun({{_X, Y} = Coord, Type}, Amphipods, Acc) when Y =:= 1 ->
    % Amphipods in the hallway can only make one move: into their final
    % destination room.
    move_to_dest(Coord, Type, Amphipods) ++ Acc;
nbr_fold_fun({{_X, Y} = Coord, Type}, Amphipods, Acc) when Y >= 2 ->
    % Amphipods in a room can move into a number of hallway positions
    lists:foldl(fun(HallwayCoord, InnerAcc) ->
                   CostToDest = cost(Coord, HallwayCoord, Type),
                   [{CostToDest, maps:put(HallwayCoord, Type, maps:remove(Coord, Amphipods))}
                    | InnerAcc]
                end,
                [],
                free_hallway_positions(Coord, Amphipods))
    ++ Acc.

move_to_dest({X, _} = Coord, Type, Amphipods) ->
    case is_dest_free(Type, Amphipods) of
        false ->
            [];
        FinalY ->
            FinalX = final_dest(Type),
            FinalCoord = {FinalX, FinalY},
            HallwayCoords =
                ?_if(X < FinalX, lists:seq(X + 1, FinalX), lists:seq(X - 1, FinalX, -1)),
            case lists:any(fun(XH) -> maps:is_key({XH, 1}, Amphipods) end, HallwayCoords) of
                true ->
                    % There was at least one amphipod blocking the path to this
                    % amphipod's destination room
                    [];
                false ->
                    CostToDest = cost({X, 1}, {FinalX, FinalY}, Type),
                    [{CostToDest, maps:put(FinalCoord, Type, maps:remove(Coord, Amphipods))}]
            end
    end.

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
% contains amphipods of the same type. Return the Y coord of the free slot , or false.
is_dest_free(Type, Amphipods) ->
    FinalX = final_dest(Type),
    case {maps:get({FinalX, 2}, Amphipods, empty), maps:get({FinalX, 3}, Amphipods, empty)} of
        {empty, empty} ->
            3;
        {empty, T} when T == Type ->
            2;
        _ ->
            false
    end.

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

is_dest_free_test() ->
    ?assertEqual(2, is_dest_free($C, #{{7, 3} => $C})),
    ?assertEqual(3, is_dest_free($C, #{{8, 1} => $C})),
    ?assertEqual(false, is_dest_free($C, #{{7, 3} => $A})).

free_hallway_positions_test() ->
    ?assertEqual([], free_hallway_positions({5, 3}, #{{5, 2} => $A})),
    ?assertEqual([], free_hallway_positions({5, 3}, #{{4, 1} => $A, {6, 1} => $B})),
    ?assertEqual([{6, 1}],
                 lists:sort(free_hallway_positions({5, 3}, #{{4, 1} => $A, {8, 1} => $B}))),
    ?assertEqual([{1, 1}, {2, 1}, {4, 1}, {6, 1}],
                 lists:sort(free_hallway_positions({5, 3}, #{{8, 1} => $B}))),
    ?assertEqual([{10, 1}, {11, 1}],
                 lists:sort(free_hallway_positions({9, 3}, #{{8, 1} => $B}))).

cost_test() ->
    ?assertEqual(3000, cost({3, 3}, {4, 1}, $D)),
    ?assertEqual(7000, cost({4, 1}, {9, 3}, $D)),
    ?assertEqual(2000, cost({3, 2}, {4, 1}, $D)).

% ex1_test() ->
%     Binary =
%         <<"#############\n#...........#\n###B#C#B#D###\n..#A#D#C#A#..\n..######"
%           "###..">>,
%     ?assertEqual(12521, solve1(parse(Binary))).

-endif.
