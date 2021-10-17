-module(aoc2018_day18).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2018,
                day = 18,
                name = "Settlers of The North Pole",
                expected = {466312, 176782},
                has_input_file = true}.

-type input_type() :: [string()].
-type result_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    string:tokens(binary_to_list(Input), "\n\r").

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    Grid = parse_lines(Input, 0, #{}),
    MaxX = 50,
    MaxY = 50,
    Iters = 10,
    G0 = do_iterations(Iters, Grid, MaxX, MaxY),
    get_resource_value(G0).

-spec solve2(Input :: input_type()) -> result_type().
solve2(_Input) ->
    cyclic_pattern(1000000000).

%% The total resource value is cyclical with a period of 28, and
%% starting at e.g. 968. This was discovered by simply printing out the
%% values and looking for the start of the loop.

-define(CYCLE_LENGTH, 28).
-define(CYCLE_BASE, 968).

iter(972) ->
    176782.

%% Only defined when N >= ?CYCLE_BASE.
cyclic_pattern(N) when N >= ?CYCLE_BASE ->
    iter((N - ?CYCLE_BASE) rem ?CYCLE_LENGTH + ?CYCLE_BASE).

get_resource_value(Grid) ->
    {SumWood, SumLumber} =
        maps:fold(fun (_, $|, {Wood, Lumber}) ->
                          {Wood + 1, Lumber};
                      (_, $#, {Wood, Lumber}) ->
                          {Wood, Lumber + 1};
                      (_, _, Acc) ->
                          Acc
                  end,
                  {0, 0},
                  Grid),
    SumWood * SumLumber.

do_iterations(0, Grid, _, _) ->
    Grid;
do_iterations(N, Grid, MaxX, MaxY) ->
    G0 = iteration(Grid, MaxX, MaxY),
    do_iterations(N - 1, G0, MaxX, MaxY).

parse_lines([], _, Grid) ->
    Grid;
parse_lines([Line | Lines], Y, Grid) ->
    G0 = parse_line(Line, 0, Y, Grid),
    parse_lines(Lines, Y + 1, G0).

parse_line([], _, _, Grid) ->
    Grid;
parse_line([$# | Rest], X, Y, Grid) ->
    G0 = maps:put({X, Y}, $#, Grid),
    parse_line(Rest, X + 1, Y, G0);
parse_line([$| | Rest], X, Y, Grid) ->
    G0 = maps:put({X, Y}, $|, Grid),
    parse_line(Rest, X + 1, Y, G0);
parse_line([$. | Rest], X, Y, Grid) ->
    G0 = maps:put({X, Y}, $., Grid),
    parse_line(Rest, X + 1, Y, G0);
parse_line([_ | Rest], X, Y, Grid) ->
    parse_line(Rest, X + 1, Y, Grid).

coords(MaxX, MaxY) ->
    [{X, Y} || X <- lists:seq(0, MaxX - 1), Y <- lists:seq(0, MaxY - 1)].

iteration(OldGrid, MaxX, MaxY) ->
    lists:foldl(fun(Pos, NewGrid) -> next(Pos, OldGrid, NewGrid) end,
                #{},
                coords(MaxX, MaxY)).

adjacent({X, Y}, Grid) ->
    lists:sort([maps:get({Xa, Ya}, Grid)
                || Xa <- [X - 1, X, X + 1],
                   Ya <- [Y - 1, Y, Y + 1],
                   {X, Y} /= {Xa, Ya},
                   maps:is_key({Xa, Ya}, Grid)]).

count(C, Adj) ->
    length(lists:filter(fun(Elem) -> Elem == C end, Adj)).

next(Pos, OldGrid, NewGrid) ->
    Acre = maps:get(Pos, OldGrid, undefined),
    Adj = adjacent(Pos, OldGrid),

    NextAcre =
        case Acre of
            %% An open acre will become filled with trees if three or
            %% more adjacent acres contained trees. Otherwise, nothing
            %% happens.
            $. ->
                Trees = count($|, Adj),
                if Trees >= 3 ->
                       $|;
                   true ->
                       $.
                end;
            %% An acre filled with trees will become a lumberyard if
            %% three or more adjacent acres were
            %% lumberyards. Otherwise, nothing happens.
            $| ->
                Lumberyards = count($#, Adj),
                if Lumberyards >= 3 ->
                       $#;
                   true ->
                       $|
                end;
            %% An acre containing a lumberyard will remain a
            %% lumberyard if it was adjacent to at least one other
            %% lumberyard and at least one acre containing
            %% trees. Otherwise, it becomes open.
            $# ->
                Lumberyards = count($#, Adj),
                Trees = count($|, Adj),
                if (Lumberyards >= 1) and (Trees >= 1) ->
                       $#;
                   true ->
                       $.
                end
        end,

    %% io:format("~p (~p) -> ~p~n", [[Acre], Adj, [NextAcre]]),
    maps:put(Pos, NextAcre, NewGrid).
