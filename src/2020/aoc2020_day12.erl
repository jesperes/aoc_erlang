%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(aoc2020_day12).

-behavior(aoc_puzzle).

-export([ parse/1
        , solve1/1
        , solve2/1
        , info/0
        ]).

-include("aoc_puzzle.hrl").

%%------------------------------------------------------------------------------
%% @doc info/0
%% Returns info about this puzzle.
%% @end
%%------------------------------------------------------------------------------
-spec info() -> aoc_puzzle().
info() ->
  #aoc_puzzle{ module = ?MODULE
             , year = 2020
             , day = 12
             , name = "Rain Risk"
             , expected = {845, 27016}
             , has_input_file = true
             }.

%%==============================================================================
%% Types
%%==============================================================================
-type input_type() :: [string()].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

%%------------------------------------------------------------------------------
%% @doc parse/1
%% Parses input file.
%% @end
%%------------------------------------------------------------------------------
-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  string:tokens(binary_to_list(Input), "\n\r").

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(Lines :: input_type()) -> result1_type().
solve1(Lines) ->
  {Xfinal, Yfinal, _} =
    lists:foldl(
      fun(S, {X, Y, Dir}) ->
          case re:run(S, "([NSEWLRF])(\\d+)", [{capture, all_but_first, list}]) of
            {match, [M1, M2]} ->
              Value = list_to_integer(M2),
              case M1 of
                "N" -> {X, Y - Value, Dir};
                "E" -> {X + Value, Y, Dir};
                "S" -> {X, Y + Value, Dir};
                "W" -> {X - Value, Y, Dir};
                "L" -> {X, Y, left(Dir, Value)};
                "R" -> {X, Y, right(Dir, Value)};
                "F" ->
                  {X0, Y0} = forward(X, Y, Value, Dir),
                  {X0, Y0, Dir}
              end
          end
      end, {0, 0, 90}, Lines),
  abs(Xfinal) + abs(Yfinal).

%%------------------------------------------------------------------------------
%% @doc solve2/1
%% Solves part 2. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve2(Lines :: input_type()) -> result2_type().
solve2(Lines) ->
  {_, {Xfinal, Yfinal}} =
    lists:foldl(
      fun(S, {WP, Ship}) ->
          {WpX, WpY} = WP,
          {ShipX, ShipY} = Ship,
          Next =
            case re:run(S, "([NSEWLRF])(\\d+)", [{capture, all_but_first, list}]) of
              {match, [M1, M2]} ->
                V = list_to_integer(M2),
                case M1 of
                  "N" -> {{WpX, WpY - V}, Ship};
                  "E" -> {{WpX + V, WpY}, Ship};
                  "S" -> {{WpX, WpY + V}, Ship};
                  "W" -> {{WpX - V, WpY}, Ship};
                  "L" -> {wp_left(WP, V), Ship};
                  "R" -> {wp_right(WP, V), Ship};
                  "F" -> {WP, {ShipX + WpX * V, ShipY + WpY * V}}
                end
            end,
          Next
      end, {{10, -1}, {0, 0}}, Lines),
  abs(Xfinal) + abs(Yfinal).

%%==============================================================================
%% Internals
%%==============================================================================

%% Helpers
left(Dir, Value) -> ((Dir + 360) - Value) rem 360.
right(Dir, Value) -> ((Dir + 360) + Value) rem 360.

forward(X, Y, Value, 0) -> {X, Y - Value};
forward(X, Y, Value, 90) -> {X + Value, Y};
forward(X, Y, Value, 180) -> {X, Y + Value};
forward(X, Y, Value, 270) -> {X - Value, Y}.

%% Rotate waypoint left, N degrees
wp_left({X, Y}, 0) -> {X, Y};
wp_left({X, Y}, N) -> wp_left({Y, -X}, N - 90).

%% Rotate waypoint right, N degrees
wp_right({X, Y}, 0) -> {X, Y};
wp_right({X, Y}, N) -> wp_right({-Y, X}, N - 90).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
