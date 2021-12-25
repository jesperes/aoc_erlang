%% Utilities to handle grids represented as a map of {X,Y} -> Data.

-module(grid).

-export([to_str/1, grid_with_path/3]).

-define(LMARGIN, 4).

%% Converts a map representing a graph to a printable string. The map
%% should contain {X, Y} => Char mappings for all printable cells.
%%
%% @param Grid  a map containing {X, Y} keys. Any keys which are not two-tuples
%%              of integers will be ignored.
-spec to_str(Grid :: map()) -> string().
to_str(Grid) ->
    {XCoords, YCoords} = coords(Grid),
    if length(XCoords) > 0 andalso length(YCoords) > 0 ->
           MinX = lists:min(XCoords),
           MinY = lists:min(YCoords),
           MaxX = lists:max(XCoords),
           MaxY = lists:max(YCoords),
           header(MinX, MaxX)
           ++ [left_margin(Y) ++ [maps:get({X, Y}, Grid, $.) || X <- lists:seq(MinX, MaxX)] ++ "\n"
               || Y <- lists:seq(MinY, MaxY)];
       true ->
           io_lib:format("Grid is empty: ~p", [Grid])
    end.

header(MinX, MaxX) ->
    io_lib:format("~*c|", [?LMARGIN - 1, 32])
    ++ lists:map(fun(N) -> N div 10 + $0 end, lists:seq(MinX, MaxX))
    ++ "\n"
    ++ io_lib:format("~*c|", [?LMARGIN - 1, 32])
    ++ lists:map(fun(N) -> N rem 10 + $0 end, lists:seq(MinX, MaxX))
    ++ "\n"
    ++ io_lib:format("~*c+~*c~n", [?LMARGIN - 1, $-, MaxX - MinX + 1, $-]).

left_margin(Y) ->
    io_lib:format("~*w|", [?LMARGIN - 1, Y]).

coords(Map) ->
    lists:unzip(
        lists:filter(fun ({X, Y}) when is_integer(X) and is_integer(Y) ->
                             true;
                         (_) ->
                             false
                     end,
                     maps:keys(Map))).

%% Merge a map representing a grid, and overlay a path on in,
%% represented as a list of {X, Y} coordinate tuples.
grid_with_path(Grid, Path, C) ->
    maps:merge(Grid,
               maps:from_list(
                   lists:map(fun(Pos) -> {Pos, C} end, Path))).
