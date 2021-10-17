-module(aoc2016_day22).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 22,
                name = "Grid Computing",
                expected = {864, 244},
                has_input_file = true}.

-type input_type() :: [map()].
-type result1_type() :: any().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    List = string:tokens(binary_to_list(Input), "\n"),
    [_, _ | Rest] = List, %% Skip first two lines
    lists:map(fun parse_line/1, Rest).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    length(find_viable_pairs(Input)).

%%
%% Part 1 was about finding "viable pairs", i.e. pairs of nodes
%% where you can move data from A to B. This was trivial (see
%% find_viable_pairs/1 below).
%%
%% In part two the goal is to move the data in the top-right node
%% into the top-left node. Data can only be moved between
%% physically adjacent nodes, so we can visualize this as a 2x2
%% grid of nodes (which happens to be 35 nodes wide and 24 nodes
%% high). Also, data can only be moved to nodes which have enough
%% space.
%%
%% After some parsing, sorting, and looking at the numbers of the
%% nodes (and after reading the hints in the puzzle), we discover
%% that there are 3 types of nodes:
%%
%% 1. There are a number of nodes which are really large and
%% really full. These have a size >= 500T, and usepercentage >=
%% 95%. These cannot move anywhere, since there is no empty node
%% large enough to hold that amount of data. These are effectively
%% "walls".
%%
%% 2. The bulk of the nodes are ~85T and 68-85% full.
%%
%% 3. One empty node 87T large {17, 22}.
%%
%% The only way to move anything is using the single empty node,
%% so it becomes similar to a 15-game where you can only move one
%% node at a time.
%%
%% To get an idea of how the grid looks, I printed it out.
%%
%% It turns out that all of the full nodes are on line 21, except
%% one semi-full node. The empty node is directly beneath the
%% wall.
%%
%% S..................................G
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% ....................................
%% .###################################
%% ................._..................
%% ....................................
%% ....................................
%%
%% At this point I realized that the simplest way would probably
%% be to manually count the number of steps.
%%
%% 1. The empty node is at (17, 22). Move to (0, 22): 17 steps.
%% 2. Move empty node to (0, 20): 2 steps.
%% 3. Move empty node to (33, 20): 34 steps.
%% 4. Move empty node to (33, 0): 20 steps.
%% 5. Move G from (34,0) to (33,0), swapping Goal/Empty: 1 step.
%% 6. Move empty node around goal to (32,0): 4 steps.
%% 7. Repeat steps 5 and 6 34 times. Empty is now at (0,0) and goal at (1, 0).
%% 8. Move goal to (0,0): 1 step.
%%
%% So, first:
%%
%% S................................._G
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% ..................................|.
%% /---------------------------------/.
%% |###################################
%% \----------------_..................
%% ....................................
%% ....................................
%%
%% Then, 34 repetitions of this sequence:
%%
%% ..._G...  -> ...G_... -> ...G....
%% ........     ........    ...._...
%%
%% ...G....  -> ...G.... -> .._G....
%% ..._....     .._.....    ........
%%
%% And the last move to move G into place
%%
%% _G... -> G_...
%% .....    .....
%%
%% Counting the moves, we get
%%
%% Total: 17 + 2 + 34 + 20 + 5 * 34 + 1 = 244.
%%

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    %% Part 2 was solved manually by looking at the plotted grid.
    ok = start(Input),
    244.

%%% Parsing stuff.
size_to_num(Str) ->
    L = length(Str),
    case lists:split(L - 1, Str) of
        {N, "T"} ->
            list_to_integer(N);
        {N, "%"} ->
            list_to_integer(N)
    end.

path_to_pos(Path) ->
    ["dev", "grid", "node", [$x | X], [$y | Y]] = string:tokens(Path, "/-"),
    {list_to_integer(X), list_to_integer(Y)}.

parse_line(Line) ->
    [Path, Size, Used, Avail, UsePercent] = string:tokens(Line, " "),
    #{path => Path,
      pos => path_to_pos(Path),
      size => size_to_num(Size),
      used => size_to_num(Used),
      avail => size_to_num(Avail),
      usepercent => size_to_num(UsePercent)}.

start(Nodes) ->
    SortedNodes =
        lists:sort(fun(#{pos := {Xa, Ya}}, #{pos := {Xb, Yb}}) -> {Ya, Xa} =< {Yb, Xb} end,
                   Nodes),

    lists:foreach(fun(#{pos := {X, Y}, usepercent := Ratio}) ->
                     if (X == 35) and (Y == 0) -> io:format("G", []);
                        (X == 0) and (Y == 0) -> io:format("S", []);
                        Ratio >= 95 -> io:format("#", []);
                        Ratio >= 50 -> io:format(".", []);
                        true -> io:format("_", [])
                     end,
                     if X == 35 -> io:format("~n", []);
                        true -> ok
                     end
                  end,
                  SortedNodes).

find_viable_pairs(Nodes) ->
    lists:filter(fun({#{used := A_used}, #{avail := B_avail}}) ->
                    (A_used /= 0) and (A_used =< B_avail)
                 end,
                 [{P1, P2} || P1 <- Nodes, P2 <- Nodes, P1 /= P2]).
