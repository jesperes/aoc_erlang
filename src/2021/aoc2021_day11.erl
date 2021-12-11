-module(aoc2021_day11).

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 11,
                name = "Dumbo Octopus",
                expected = {1675, 515},
                has_input_file = true,
                use_one_solver_fun = true}.

-type input_type() :: map().
-type result_type() :: {integer(), integer()}.

-define(WIDTH, 10).

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    lists:foldl(fun(Key, Map) -> maps:put(Key, read_pos(Key, ?WIDTH + 1, Binary), Map) end,
                #{},
                [{X, Y} || Y <- lists:seq(0, ?WIDTH - 1), X <- lists:seq(0, ?WIDTH - 1)]).

read_pos({X, Y}, Width, Binary) ->
    binary:at(Binary, Y * Width + X) - $0.

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    iterate(Input, 1, 0, undef).

iterate(Map, N, Flashes, P1) ->
    {Map0, Flashes0, FlashedSet} = do_one_step(Map, Flashes),
    NewP1 =
        case N of
            100 ->
                Flashes0;
            _ ->
                P1
        end,
    case sets:size(FlashedSet) of
        100 ->
            {NewP1, N};
        _ ->
            iterate(Map0, N + 1, Flashes0, NewP1)
    end.

do_one_step(Map, Flashes) ->
    %% First, increase all values by 1
    MapIncr1 = incr_all(Map),

    {MapOut, FlashesOut, FlashedSet} =
        lists:foldl(fun maybe_flash/2, {MapIncr1, Flashes, sets:new()}, maps:keys(MapIncr1)),

    ResetMap =
        maps:map(fun (_, V) when V > 9 ->
                         0;
                     (_, V) ->
                         V
                 end,
                 MapOut),

    {ResetMap, FlashesOut, FlashedSet}.

maybe_flash(Coord, {Map, Count, Flashed} = State) ->
    case {sets:is_element(Coord, Flashed), maps:get(Coord, Map)} of
        {false, N} when N > 9 ->
            do_flash(Coord, {Map, Count + 1, sets:add_element(Coord, Flashed)});
        _ ->
            State
    end.

do_flash(Coord, State) ->
    lists:foldl(fun(Adj, {Map0, Count0, Flashed0} = State0) ->
                   case maps:is_key(Adj, Map0) of
                       false -> State0;
                       true ->
                           Map1 = maps:update_with(Adj, fun incr/1, Map0),
                           maybe_flash(Adj, {Map1, Count0, Flashed0})
                   end
                end,
                State,
                adjacent(Coord)).

incr(N) ->
    N + 1.

incr_all(Map) ->
    maps:fold(fun(Coord, _, Acc) -> maps:update_with(Coord, fun incr/1, Acc) end, Map, Map).

adjacent({X, Y}) ->
    [{X - 1, Y - 1},
     {X, Y - 1},
     {X + 1, Y - 1},
     {X - 1, Y},
     {X + 1, Y},
     {X - 1, Y + 1},
     {X, Y + 1},
     {X + 1, Y + 1}].

%% Tests

-ifdef(TEST).

ex1_test() ->
    Binary =
        <<"5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n41675246"
          "45\n2176841721\n6882881134\n4846848554\n5283751526\n">>,
    Input = parse(Binary),
    {1656, 195} = solve(Input).

-endif.
