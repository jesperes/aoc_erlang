%%% Advent of Code solution for 2019 day 11.
%%% Created: 2019-12-11T05:34:19+00:00

-module(aoc2019_day11).

-behavior(aoc_puzzle).

%% Terminology
%% HPR - Hull Painting Robot
%% IRP - Intcode Refactoring Project
%% AOC - Awesome Or Crap?

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 11,
                name = "Space Police",
                expected =
                    {2343,
                     %% Kludge to trick code formatter
                     lists:flatten(["   ## #### ###  #### ###  ###  #  # #  #   \n",
                                    "    # #    #  # #    #  # #  # #  # #  #   \n",
                                    "    # ###  ###  ###  #  # ###  #  # ####   \n",
                                    "    # #    #  # #    ###  #  # #  # #  #   \n",
                                    " #  # #    #  # #    # #  #  # #  # #  #   \n",
                                    "  ##  #    ###  #### #  # ###   ##  #  #   \n"])},
                has_input_file = true}.

-define(WHITE, 1).
-define(BLACK, 0).

-type input_type() :: intcode:intcode_program().
-type result1_type() :: integer().
-type result2_type() :: string().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    intcode:parse(Binary).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Prog) ->
    {_, Grid} =
        intcode:execute(Prog,
                        fun input/1,
                        fun output/2,
                        #{pos => {0, 0},
                          dir => 0,
                          next_output => paint}),

    maps:size(
        maps:filter(fun ({_, _}, _) ->
                            true;
                        (_, _) ->
                            false
                    end,
                    Grid)).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Prog) ->
    {_, Grid} =
        intcode:execute(Prog,
                        fun input/1,
                        fun output/2,
                        #{pos => {0, 0},
                          dir => 0,
                          next_output => paint,
                          {0, 0} => ?WHITE}),
    lists:flatten(grid_to_str(Grid)).

grid_to_str(Grid) ->
    Coords =
        lists:filter(fun ({_, _}) ->
                             true;
                         (_) ->
                             false
                     end,
                     maps:keys(Grid)),
    Xs = lists:map(fun({X, _}) -> X end, Coords),
    Ys = lists:map(fun({_, Y}) -> Y end, Coords),

    [[case maps:get({X, Y}, Grid, undef) of
          ?WHITE ->
              $#;
          _ ->
              32
      end
      || X
             <- lists:seq(
                    lists:min(Xs), lists:max(Xs))]
     ++ "\n"
     || Y
            <- lists:seq(
                   lists:min(Ys), lists:max(Ys))].

input(State) ->
    %% This function is used to access the HPR camera to to see what
    %% color tile we are currently over.
    Pos = maps:get(pos, State),
    Color = maps:get(Pos, State, ?BLACK),
    {State, Color}.

output(Output, State) ->
    %% Output function paints the HPR
    Pos = maps:get(pos, State),
    case maps:get(next_output, State) of
        paint ->
            Color = Output,
            State0 = maps:put(Pos, Color, State),
            maps:put(next_output, turn, State0);
        turn ->
            Delta = Output * 2 - 1, %% 0 -> left (-1), 1 -> right (+1)
            Dir = (maps:get(dir, State) + Delta + 4) rem 4,
            State0 = maps:put(dir, Dir, State),
            NewPos = move(Pos, Dir),
            State1 = maps:put(pos, NewPos, State0),
            maps:put(next_output, paint, State1)
    end.

move({X, Y}, 0) ->
    {X, Y - 1};
move({X, Y}, 1) ->
    {X + 1, Y};
move({X, Y}, 2) ->
    {X, Y + 1};
move({X, Y}, 3) ->
    {X - 1, Y}.
