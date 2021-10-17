-module(aoc2016_day01).

-define(INPUT,
        <<"L1, R3, R1, L5, L2, L5, R4, L2, R2, R2, L2, R1, L5, R3, L4, "
          "L1, L2, R3, R5, L2, R5, L1, R2, L5, R4, R2, R2, L1, L1, R1, "
          "L3, L1, R1, L3, R5, R3, R3, L4, R4, L2, L4, R1, R1, L193, R2, "
          "L1, R54, R1, L1, R71, L4, R3, R191, R3, R2, L4, R3, R2, L2, "
          "L4, L5, R4, R1, L2, L2, L3, L2, L1, R4, R1, R5, R3, L5, R3, "
          "R4, L2, R3, L1, L3, L3, L5, L1, L3, L3, L1, R3, L3, L2, R1, "
          "L3, L1, R5, R4, R3, R2, R3, L1, L2, R4, L3, R1, L1, L1, R5, "
          "R2, R4, R5, L1, L1, R1, L2, L4, R3, L1, L3, R5, R4, R3, R3, "
          "L2, R2, L1, R4, R2, L3, L4, L2, R2, R2, L4, R3, R5, L2, R2, "
          "R4, R5, L2, L3, L2, R5, L4, L2, R3, L5, R2, L1, R1, R3, R3, "
          "L5, L2, L2, R5">>).

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 1,
                name = "No Time for a Taxicab",
                expected = {278, 161},
                use_one_solver_fun = true,
                has_input_file = false}.

-type input_type() :: [string()].
-type result_type() :: {integer(), integer()}.

-spec parse(Input :: binary()) -> input_type().
parse(_) ->
    string:tokens(binary_to_list(?INPUT), ", ").

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    {FinalPos, _, FinalVT, _} =
        lists:foldl(fun([Turn | Rest], {Pos, Dir, VT, Visited}) ->
                       NewDir = turn(Turn, Dir),
                       {NewPos, VT0, V0} =
                           visit(Pos, delta(NewDir), list_to_integer(Rest), VT, Visited),
                       {NewPos, NewDir, VT0, V0}
                    end,
                    {{0, 0}, 0, undef, sets:new()},
                    Input),

    {dist(FinalPos), dist(FinalVT)}.

dist({X, Y}) ->
    abs(X) + abs(Y).

turn($R, Dir) ->
    (Dir + 1) rem 4;
turn($L, Dir) ->
    (Dir + 3) rem 4.

delta(0) ->
    {0, -1};
delta(1) ->
    {1, 0};
delta(2) ->
    {0, 1};
delta(3) ->
    {-1, 0}.

visit(Pos, _, 0, VT, Visited) ->
    {Pos, VT, Visited};
visit({X, Y}, {Dx, Dy}, Steps, undef, Visited) ->
    VT = case sets:is_element({X, Y}, Visited) of
             true ->
                 {X, Y};
             false ->
                 undef
         end,
    visit({X + Dx, Y + Dy}, {Dx, Dy}, Steps - 1, VT, sets:add_element({X, Y}, Visited));
visit({X, Y}, {Dx, Dy}, Steps, VT, Visited) ->
    visit({X + Dx, Y + Dy}, {Dx, Dy}, Steps - 1, VT, sets:add_element({X, Y}, Visited)).
