%%% Advent of Code solution for 2019 day 20.
%%% Created: 2019-12-31T15:41:26+00:00

-module(aoc2019_day20).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 20,
                name = "Donut Maze",
                expected = {442, 5208},
                has_input_file = true}.

-type maze() :: {Grid :: map(), Portals :: map()}.
-type input_type() :: maze().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    Lines = string:tokens(binary_to_list(Binary), "\n"),
    Map = lists:foldl(fun({Y, Line}, Acc) -> parse_line(Y, Line, Acc) end,
                      #{},
                      lists:zip(
                          lists:seq(0, length(Lines) - 1), Lines)),
    {Map, find_portals(Map)}.

-spec solve1(Input :: input_type()) -> result_type().
solve1(Maze) ->
    {_, Portals} = Maze,
    StartPos = maps:get('AA', Portals),
    {found, Node, State} = dijkstra:dijkstra(Maze, StartPos, fun neighbors/2),
    Path = dijkstra:shortest_path(State, Node),
    length(Path) - 1.

-spec solve2(Input :: input_type()) -> result_type().
solve2(Maze) ->
    {_, Portals} = Maze,
    StartPos = {0, maps:get('AA', Portals)},
    {found, Node, State} = dijkstra:dijkstra(Maze, StartPos, fun neighbors2/2),
    Path = dijkstra:shortest_path(State, Node),
    length(Path) - 1.

%% Part 1

neighbors({_, _} = Pos, {Map, Portals}) ->
    case maps:get('ZZ', Portals) of
        End when End =:= Pos ->
            found;
        _ ->
            lists:filtermap(fun(AdjPos) ->
                               case maps:get(AdjPos, Map, undef) of
                                   $. -> {true, {1, AdjPos}};
                                   _ -> false
                               end
                            end,
                            adj(Pos))
            ++ %% Add the portal as a neighbor, if there is one.
               case maps:get(Pos, Portals, undef) of
                   undef ->
                       [];
                   Portal ->
                       [{1, Portal}]
               end
    end.

%% Part 2

%% For part 2 we need to track which level we are on, and disallow
%% moving outside the top-most level.
neighbors2({Level, {_, _} = Pos}, {Map, Portals}) ->
    case maps:get('ZZ', Portals) of
        End when (End =:= Pos) and (Level == 0) ->
            found;
        _ ->
            lists:filtermap(fun(AdjPos) ->
                               case maps:get(AdjPos, Map, undef) of
                                   $. -> {true, {1, {Level, AdjPos}}};
                                   _ -> false
                               end
                            end,
                            adj(Pos))
            ++ %% Add the portal as a neighbor, if there is one.
               case maps:get(Pos, Portals, undef) of
                   undef ->
                       [];
                   Portal ->
                       %% Track movement to adjacent levels.
                       NextLevel =
                           case maps:get({type, Pos}, Portals) of
                               outer ->
                                   Level - 1;
                               inner ->
                                   Level + 1
                           end,
                       %% Don't allow teleporting outside the outermost level.
                       if NextLevel >= 0 ->
                              [{1, {NextLevel, Portal}}];
                          true ->
                              []
                       end
               end
    end.

adj({X, Y}) ->
    [{X - 1, Y}, {X + 1, Y}, {X, Y - 1}, {X, Y + 1}].

%% ==================================================================
%% Code for parsing the maze. This is a bit more work than usual,
%% because we need to locate all the portals.
%% ==================================================================

%%
%% Portals can either be
%%
%% Right: AB.
%%
%% Left: .AB
%%
%% Up: .
%%     A
%%     B
%%
%% Down: A
%%       B
%%       .
%%
%% Portals always read in natural order (Up->Down or Left->Right)
%%

parse_line(Y, Line, Acc) ->
    lists:foldl(fun ({X, D}, InnerAcc) when (D =:= $#) or (D =:= $.) ->
                        maps:put({X, Y}, D, InnerAcc);
                    ({X, D}, InnerAcc) when (D >= $A) and (D =< $Z) ->
                        Pos = {X, Y},
                        maps:put(Pos,
                                 D,
                                 maps:update_with(D, fun(Old) -> [Pos | Old] end, [Pos], InnerAcc));
                    (_, InnerAcc) ->
                        InnerAcc
                end,
                Acc,
                lists:zip(
                    lists:seq(0, length(Line) - 1), Line)).

find_portals(Map) ->
    Size = get_maze_size(Map),

    Portals =
        lists:foldl(fun(C, Acc) ->
                       Portals = find_portals(C, Map),
                       lists:foldl(fun({Name, Pos}, InnerAcc) ->
                                      case maps:get(Name, InnerAcc, undef) of
                                          undef -> maps:put(Name, Pos, InnerAcc);
                                          OtherPos ->
                                              %% For part 2 we need to be able to tell if a
                                              %% portal is on the inner or outer side of the
                                              %% donut so that we can track the level.
                                              maps:merge(InnerAcc,
                                                         #{Pos => OtherPos,
                                                           OtherPos => Pos,
                                                           {type, Pos} => portal_type(Pos, Size),
                                                           {type, OtherPos} =>
                                                               portal_type(OtherPos, Size)})
                                      end
                                   end,
                                   Acc,
                                   Portals)
                    end,
                    #{},
                    lists:seq($A, $Z)),

    %% Filter out one-sided "portals" (i.e. 'AA' and 'ZZ').
    maps:filter(fun (K, V) when K =:= V ->
                        false;
                    (_, _) ->
                        true
                end,
                Portals).

%% Figure out which direction a portal goes. This is necessary so that
%% we know if we are going inwards or outwards.
portal_type({2, _}, _) ->
    outer;
portal_type({_, 2}, _) ->
    outer;
portal_type({X, _}, {W, _}) when X == W - 3 ->
    outer;
portal_type({_, Y}, {_, H}) when Y == H - 3 ->
    outer;
portal_type(_, _) ->
    inner.

%% Return the {Width,Height} of the maze.
get_maze_size(Map) ->
    {XCoords, YCoords} =
        lists:unzip(
            maps:keys(
                maps:filter(fun ({X, Y}, _) ->
                                    is_integer(X) and is_integer(Y);
                                (_, _) ->
                                    false
                            end,
                            Map))),
    MaxX = lists:max(XCoords),
    MaxY = lists:max(YCoords),
    {MaxX + 1, MaxY + 1}.

%% Find all the portals beginning with Letter.
find_portals(Letter, Map) ->
    case maps:get(Letter, Map, undefined) of
        undefined ->
            [];
        LetterPos ->
            lists:filtermap(fun(Pos) -> get_portal(Letter, Pos, Map) end, LetterPos)
    end.

%% If Letter is the first letter of a portal, returns {true, {Name,
%% Pos}} where Name is the name of the portal (e.g. AA), and Pos is
%% the portal location (i.e. the tile immediately outside the '.'.
get_portal(Letter, {X, Y}, Map) ->
    ToA = fun list_to_atom/1,

    %% Check if the key is vertical or horizontal
    IsVert = not maps:is_key({X + 1, Y}, Map) and not maps:is_key({X - 1, Y}, Map),

    IsHoriz = not maps:is_key({X, Y - 1}, Map) and not maps:is_key({X, Y + 1}, Map),

    if IsVert ->
           Downwards =
               (maps:get({X, Y + 2}, Map, undef) =:= $.) and not maps:is_key({X, Y - 1}, Map),
           Upwards = (maps:get({X, Y - 2}, Map, undef) =:= $.) and not maps:is_key({X, Y + 1}, Map),
           if Downwards ->
                  Letter2 = maps:get({X, Y + 1}, Map),
                  {true, {ToA([Letter, Letter2]), {X, Y + 2}}};
              Upwards ->
                  Letter2 = maps:get({X, Y - 1}, Map),
                  {true, {ToA([Letter2, Letter]), {X, Y - 2}}};
              true ->
                  false
           end;
       IsHoriz ->
           Right = (maps:get({X + 2, Y}, Map, undef) =:= $.) and not maps:is_key({X - 1, Y}, Map),
           Left = (maps:get({X - 2, Y}, Map, undef) =:= $.) and not maps:is_key({X + 1, Y}, Map),
           if Right ->
                  Letter2 = maps:get({X + 1, Y}, Map),
                  {true, {ToA([Letter, Letter2]), {X + 2, Y}}};
              Left ->
                  Letter2 = maps:get({X - 1, Y}, Map),
                  {true, {ToA([Letter2, Letter]), {X - 2, Y}}};
              true ->
                  false
           end;
       true ->
           false
    end.
