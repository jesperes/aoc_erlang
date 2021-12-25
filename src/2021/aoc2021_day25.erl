-module(aoc2021_day25).

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-compile([export_all, nowarn_export_all]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 25,
                name = "Sea Cucumber",
                expected = 295,
                has_input_file = true,
                use_one_solver_fun = true}.

-type input_type() :: any().
-type result_type() :: integer().

to_coords(Binary, Width, Char) ->
    lists:foldl(fun({Offset, _}, Map) ->
                   maps:put({Offset rem (Width + 1), Offset div (Width + 1)}, Char, Map)
                end,
                #{},
                binary:matches(Binary, <<Char>>)).

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    {Width, _} = binary:match(Binary, <<"\n">>),
    Height = size(Binary) div (Width + 1),
    East = to_coords(Binary, Width, $>),
    South = to_coords(Binary, Width, $v),
    {maps:merge(East, South), {Width, Height}}.

-spec solve(Input :: input_type()) -> result_type().
solve({Herds, Size}) ->
    do_until_no_moves(Herds, Size, 1).

do_until_no_moves(Herds, Size, N) ->
    Herds0 = move_herd(Herds, Size, $>),
    Herds1 = move_herd(Herds0, Size, $v),
    ?_if(Herds1 =:= Herds, N, do_until_no_moves(Herds1, Size, N + 1)).

move_herd(Herds, Size, Type) ->
    % Find which sea cucumbers can move
    Movables =
        maps:fold(fun (Coord, T, Acc) when T =:= Type ->
                          AdjCoord = adj(Coord, T, Size),
                          ?_if(not maps:is_key(AdjCoord, Herds), [{Coord, AdjCoord, T} | Acc], Acc);
                      (_, _, Acc) ->
                          Acc
                  end,
                  [],
                  Herds),

    % Move them
    lists:foldl(fun({Coord, AdjCoord, T}, Acc) ->
                   maps:put(AdjCoord, T, maps:remove(Coord, Acc))
                end,
                Herds,
                Movables).

adj({X, Y}, $>, {Width, _Height}) ->
    {(X + 1) rem Width, Y};
adj({X, Y}, $v, {_Width, Height}) ->
    {X, (Y + 1) rem Height}.
