-module(aoc2018_day10).

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-include("aoc_puzzle.hrl").

%% Avoid breaking formatting in text_block/1
-format #{paper => 100, ribbon => 100}.

text_block() ->
  ["   ###     ###  #    #  ######  #    #  #    #  ######  #####  ",
   "    #       #   #    #       #  #    #  #   #   #       #    # ",
   "    #       #    #  #        #  #    #  #  #    #       #    # ",
   "    #       #    #  #       #   #    #  # #     #       #    # ",
   "    #       #     ##       #    ######  ##      #####   #####  ",
   "    #       #     ##      #     #    #  ##      #       #      ",
   "    #       #    #  #    #      #    #  # #     #       #      ",
   "#   #   #   #    #  #   #       #    #  #  #    #       #      ",
   "#   #   #   #   #    #  #       #    #  #   #   #       #      ",
   " ###     ###    #    #  ######  #    #  #    #  #       #      "].

-spec info() -> aoc_puzzle().
info() ->
  #aoc_puzzle{module = ?MODULE,
              year = 2018,
              day = 10,
              name = "The Stars Align",
              expected = {text_block(), 10036},
              use_one_solver_fun = true,
              has_input_file = true}.

-type input_type() :: map().
-type result_type() :: {string(), integer()}.

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  Str = binary_to_list(Input),
  Points =
    lists:map(fun(Line) ->
                 ["position", X, Y, "velocity", Dx, Dy] = string:tokens(Line, "=<, >"),
                 {{to_i(X), to_i(Y)}, {to_i(Dx), to_i(Dy)}}
              end,
              string:tokens(Str, "\n")),

  lists:foldl(fun({Pos, Vel}, Map) -> add_point_at(Pos, Vel, Map) end, maps:new(), Points).

-spec solve(Input :: input_type()) -> result_type().
solve(Points) ->
  {Map, Seconds} = do_steps(Points, 0),
  Coords = maps:keys(Map),
  {OffsetX, OffsetY} =
    lists:foldl(fun({X, Y}, {X0, Y0}) -> {min(X, X0), min(Y, Y0)} end, {inf, inf}, Coords),
  Width = 62,
  Height = 9,
  TextBlock =
    [[map_to_char(X, Y, Map) || X <- lists:seq(OffsetX, Width + OffsetX)]
     || Y <- lists:seq(OffsetY, Height + OffsetY)],
  {TextBlock, Seconds}.

map_to_char(X, Y, Map) ->
  case maps:is_key({X, Y}, Map) of
    true ->
      $#;
    false ->
      32
  end.

to_i(Str) ->
  list_to_integer(Str).

do_steps(Points, Sec) ->
  P1 = step_points(Points),
  W1 = get_width(Points),
  W2 = get_width(P1),
  %% When the width of the bounding box starts to expand, we are
  %% done. This assumes that all points converge to form the
  %% message, and the message appear when the points are closest
  %% together.
  IsExpanding = abs(W2) > abs(W1),
  if IsExpanding ->
       {Points, Sec};
     true ->
       do_steps(P1, Sec + 1)
  end.

get_width(P) ->
  {MinX, MaxX, _, _} = get_bounds(P),
  MaxX - MinX.

add_point_at(Pos, Vel, Map) ->
  maps:update_with(Pos, fun(V) -> [Vel | V] end, [Vel], Map).

getmax(X, undef) ->
  X;
getmax(X, Max) when X > Max ->
  X;
getmax(_, Max) ->
  Max.

getmin(X, undef) ->
  X;
getmin(X, Min) when X < Min ->
  X;
getmin(_, Min) ->
  Min.

get_bounds(Points) ->
  maps:fold(fun({X, Y}, _, {MaxX, MinX, MaxY, MinY}) ->
               {getmax(X, MaxX), getmin(X, MinX), getmax(Y, MaxY), getmin(Y, MinY)}
            end,
            {undef, undef, undef, undef},
            Points).

step_points(Points) ->
  maps:fold(fun({X, Y}, Velocities, Map) ->
               lists:foldl(fun({Dx, Dy} = Vel, M) ->
                              NewPos = {X + Dx, Y + Dy},
                              add_point_at(NewPos, Vel, M)
                           end,
                           Map,
                           Velocities)
            end,
            maps:new(),
            Points).
