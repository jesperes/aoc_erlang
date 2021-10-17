%%% Advent of Code solution for 2019 day 18.
%%% Created: 2019-12-18T18:36:27+00:00

-module(aoc2019_day18).

-include_lib("stdlib/include/assert.hrl").

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 18,
                name = "Many-Worlds Intepretation",
                expected = {3856, 1660},
                has_input_file = true}.

-type input_type() ::
    {{Width :: integer(), Height :: integer()}, AllKeys :: binary(), Grid :: map()}.
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    [{Width, _} | _] = binary:matches(Binary, <<"\n">>),
    ?assertEqual($\n, binary:at(Binary, Width)),
    Height = byte_size(Binary) div (Width + 1),
    AllKeys = usort(find_all_keys(Binary)),
    {{Width, Height}, AllKeys, parse(Binary, 0, Width, #{})}.

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    {_, AllKeys, Grid} = Input,
    [StartPos] = find_start_points(Grid),
    Grid0 = maps:put(keys, AllKeys, Grid),
    {found, Node, State} = dijkstra:dijkstra(Grid0, {StartPos, <<>>}, fun eval1/2),
    length(dijkstra:shortest_path(State, Node)) - 1.

%%
%% Part 2. We assume that each bot can ignore doors for which there
%% are no keys in that quadrant. When encountering such a door, the
%% bot will simply "wait" until the key is found by another bot.  We
%% run the searches independently in each of the four quadrants, and
%% sum up the shortest paths.
%%
%% ...      @#@
%% .@.  =>  ###
%% ...      @#@
%%
-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    {_, _, Grid} = Input,
    [Center] = find_start_points(Grid),
    Grid0 = patch_center(Grid, Center),
    Bots = lists:sort(find_start_points(Grid0)),
    Quadrants = lists:zip([nw, sw, ne, se], Bots),
    lists:foldl(fun({Quadrant, BotPos}, Acc) ->
                   {KeysToFind, Doors} = quadrant(Quadrant, Center, Grid0),

                   DoorsStr = binary_to_list(Doors),
                   KeysStr = binary_to_list(KeysToFind),

                   DoorsToIgnore =
                       lists:filter(fun(C) -> not lists:member(C + 32, KeysStr) end, DoorsStr),

                   EvalFun =
                       fun({Pos, KeysIn}, Graph) ->
                          case KeysIn =:= KeysToFind of
                              true -> found;
                              false ->
                                  lists:foldl(fun(Adj, InnerAcc) ->
                                                 case maps:get(Adj, Graph) of
                                                     $# -> InnerAcc;
                                                     C when (C =:= $.) or (C =:= $@) ->
                                                         [{1, {Adj, KeysIn}} | InnerAcc];
                                                     C when (C >= $A) and (C =< $Z) ->
                                                         case lists:member(C, DoorsToIgnore) of
                                                             true ->
                                                                 [{1, {Adj, KeysIn}} | InnerAcc];
                                                             false ->
                                                                 LC = list_to_binary([C + 32]),
                                                                 case binary:match(KeysIn, LC) of
                                                                     nomatch -> Acc;
                                                                     _ ->
                                                                         [{1, {Adj, KeysIn}}
                                                                          | InnerAcc]
                                                                 end
                                                         end;
                                                     C when (C >= $a) and (C =< $z) ->
                                                         Str = binary_to_list(KeysIn),
                                                         B = list_to_binary(lists:usort([C | Str])),
                                                         [{1, {Adj, B}} | InnerAcc]
                                                 end
                                              end,
                                              [],
                                              adj(Pos))
                          end
                       end,

                   {found, Node, State} = dijkstra:dijkstra(Grid0, {BotPos, <<>>}, EvalFun),
                   Acc + dijkstra:shortest_dist(State, Node)
                end,
                0,
                Quadrants).

%% Search callback function for part 1. See dijkstra.erl.
eval1({Pos, KeysIn}, Graph) ->
    case KeysIn =:= maps:get(keys, Graph) of
        true ->
            found;
        false ->
            lists:foldl(fun(Adj, Acc) ->
                           case maps:get(Adj, Graph) of
                               $# -> Acc;
                               C when (C =:= $.) or (C =:= $@) -> [{1, {Adj, KeysIn}} | Acc];
                               C when (C >= $A) and (C =< $Z) ->
                                   LC = list_to_binary([C + 32]),
                                   case binary:match(KeysIn, LC) of
                                       nomatch -> Acc;
                                       _ -> [{1, {Adj, KeysIn}} | Acc]
                                   end;
                               C when (C >= $a) and (C =< $z) ->
                                   Str = binary_to_list(KeysIn),
                                   B = list_to_binary(lists:usort([C | Str])),
                                   [{1, {Adj, B}} | Acc]
                           end
                        end,
                        [],
                        adj(Pos))
    end.

%% Return a list of {Keys,Doors} in the given quadrant.
quadrant(Quadrant, Center, Grid) ->
    {Keys, Doors} =
        maps:fold(fun(K, C, {Kin, Din} = Acc) ->
                     case is_quadrant(Quadrant, K, Center) of
                         false -> Acc;
                         true ->
                             if (C >= $a) and (C =< $z) -> {<<C, Kin/binary>>, Din};
                                (C >= $A) and (C =< $Z) -> {Kin, <<C, Din/binary>>};
                                true -> Acc
                             end
                     end
                  end,
                  {<<>>, <<>>},
                  Grid),
    {usort(Keys), usort(Doors)}.

%% ============================================================
%% Utility functions
%% ============================================================

find_all_keys(<<>>) ->
    <<>>;
find_all_keys(<<K, Rest/binary>>) when (K >= $a) and (K =< $z) ->
    <<K, (find_all_keys(Rest))/binary>>;
find_all_keys(<<_, Rest/binary>>) ->
    find_all_keys(Rest).

usort(List) when is_list(List) ->
    lists:usort(List);
usort(Bin) when is_binary(Bin) ->
    list_to_binary(lists:usort(binary_to_list(Bin))).

adj({X, Y}) ->
    [{X - 1, Y}, {X + 1, Y}, {X, Y + 1}, {X, Y - 1}].

is_quadrant(nw, {X, Y}, {Xc, Yc}) ->
    (X < Xc) and (Y < Yc);
is_quadrant(ne, {X, Y}, {Xc, Yc}) ->
    (X > Xc) and (Y < Yc);
is_quadrant(sw, {X, Y}, {Xc, Yc}) ->
    (X < Xc) and (Y > Yc);
is_quadrant(se, {X, Y}, {Xc, Yc}) ->
    (X > Xc) and (Y > Yc).

%% ============================================================
%% Parse helpers
%% ============================================================

parse(<<>>, _, _Width, Grid) ->
    Grid;
parse(<<$\n, Rest/binary>>, N, Width, Grid) ->
    parse(Rest, N + 1, Width, Grid);
parse(<<C, Rest/binary>>, N, Width, Grid) ->
    Pos = xy_from_offset(N, Width),
    parse(Rest, N + 1, Width, maps:put(Pos, C, Grid)).

xy_from_offset(N, Width) ->
    {N rem (Width + 1), N div (Width + 1)}.

find_start_points(Grid) ->
    lists:filtermap(fun ({Pos, $@}) ->
                            {true, Pos};
                        (_) ->
                            false
                    end,
                    maps:to_list(Grid)).

patch_center(Grid, {X, Y}) ->
    maps:merge(Grid,
               #{{X - 1, Y - 1} => $@,
                 {X, Y - 1} => $#,
                 {X + 1, Y - 1} => $@,
                 {X - 1, Y} => $#,
                 {X, Y} => $#,
                 {X + 1, Y} => $#,
                 {X - 1, Y + 1} => $@,
                 {X, Y + 1} => $#,
                 {X + 1, Y + 1} => $@}).
