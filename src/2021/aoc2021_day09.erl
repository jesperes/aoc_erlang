-module(aoc2021_day09).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 9,
                name = "Smoke Basin",
                expected = {524, 1235430},
                has_input_file = true}.

-type input_type() :: {Width :: integer(), Height :: integer(), Binary :: binary()}.
-type result_type() :: any(). %integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    {Width, _} = binary:match(Binary, <<"\n">>),
    Height = size(Binary) div (Width + 1),
    {Width, Height, Binary}.

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    lists:foldl(fun({X, Y}, Acc) -> Acc + read_pos(X, Y, Input) + 1 - $0 end,
                0,
                find_all_low_points(Input)).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    LowPoints = find_all_low_points(Input),
    [A, B, C | _] =
        lists:reverse(
            lists:sort(
                lists:map(fun(LP) -> sets:size(fill(LP, sets:from_list([LP]), Input)) end,
                          LowPoints))),
    A * B * C.

fill({X, Y}, Basin, Input) ->
    lists:foldl(fun({Ax, Ay} = AdjPos, Acc) ->
                   case read_pos(Ax, Ay, Input) of
                       $9 -> Acc;
                       _ ->
                           case sets:is_element(AdjPos, Acc) of
                               true -> Acc;
                               false -> fill(AdjPos, sets:add_element(AdjPos, Acc), Input)
                           end
                   end
                end,
                Basin,
                valid_adjacents(X, Y, Input)).

%% Helpers

all_coords({Width, Height, _}) ->
    [{X, Y} || Y <- lists:seq(0, Height - 1), X <- lists:seq(0, Width - 1)].

find_all_low_points(Input) ->
    lists:filter(fun(Coord) -> is_low_point(Coord, Input) end, all_coords(Input)).

is_low_point({X, Y}, Input) ->
    lower_adjacent_points(X, Y, Input) =:= [].

adjacents(X, Y) ->
    [{X - 1, Y}, {X, Y - 1}, {X + 1, Y}, {X, Y + 1}].

valid_adjacents(X, Y, Input) ->
    [{Ax, Ay} || {Ax, Ay} <- adjacents(X, Y), valid_coords(Ax, Ay, Input)].

lower_adjacent_points(X, Y, Input) ->
    HeightAtPos = read_pos(X, Y, Input),
    [{Ax, Ay}
     || {Ax, Ay} <- valid_adjacents(X, Y, Input), read_pos(Ax, Ay, Input) =< HeightAtPos].

valid_coords(X, Y, {Width, Height, _})
    when X >= 0 andalso Y >= 0 andalso Y < Height andalso X < Width ->
    true;
valid_coords(_, _, _) ->
    false.

read_pos(X, Y, {Width, _, Binary}) ->
    binary:at(Binary, Y * (Width + 1) + X).
