-module(aoc2021_day23).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

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
    Letters = coord_map(Binary, "ABCD"),
    {Walls, Letters}.

-spec solve1(Input :: input_type()) -> result_type().
solve1({Walls, Letters}) ->
    io:format(standard_error,
              "~s~n",
              [grid:to_str(
                   maps:merge(Walls, Letters))]),
    Explored = sets:new(),
    Frontier = gb_sets:from_list([{0, Letters}]),
    find_shortest_path(Explored, Frontier, Walls),
    0.

-spec solve2(Input :: input_type()) -> result_type().
solve2(_Input) ->
    0.

% 17478

%% Dijkstra
find_shortest_path(Explored, Frontier, Walls) ->
    {{Cost, Letters} = Node, Frontier0} = gb_sets:take_smallest(Frontier),
    case Letters =:= goal() of
        true ->
            Cost;
        false ->
            Explored0 = sets:add_element(Letters, Explored),
            lists:foldl(fun(Nbr, FrontierIn) ->
                           case sets:is_element(Nbr, Explored0) of
                               true -> FrontierIn;
                               false -> gb_sets:add(Nbr, FrontierIn)
                           end
                        end,
                        Frontier0,
                        neighbors(Node, Walls))
    end.

neighbors({_Cost, _Letters}, _Walls) ->
    % Cost = the cost/distance from the start to this point
    % Letters = the current positions of the letters (Coord => Char map)
    % Walls = the position of the walls
    [].

%% Tests
-ifdef(TEST).

%% ...

-endif.
