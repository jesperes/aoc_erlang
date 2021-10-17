%%% Advent of Code solution for 2019 day 24.
%%% Created: 2019-12-24T11:21:51+00:00

-module(aoc2019_day24).

-behavior(aoc_puzzle).

-hank([{unnecessary_function_arguments, [{game_of_life2, 2, 1}]}]).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 24,
                name = "Planet of Discord",
                expected = {17863711, 1937},
                has_input_file = false}.

-type input_type() :: binary().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(_) ->
    <<"...#.", "#.##.", "#..##", "#.###", "##...">>.

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    part1(Input).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    part2(Input, 200).

%% ------------------------------------------------------------
%% Part 1: game of life-like cellular atomaton.
%% ------------------------------------------------------------

part1(Input) ->
    State = find_first_repeated_state(parse_part1(Input), sets:new()),
    biodiv_rating(State).

find_first_repeated_state(State, SeenStates) ->
    NewState = game_of_life(State),
    case sets:is_element(NewState, SeenStates) of
        true ->
            NewState;
        false ->
            find_first_repeated_state(NewState, sets:add_element(NewState, SeenStates))
    end.

biodiv_rating(State) ->
    lists:foldl(fun(Off, Acc) ->
                   X = Off rem 5,
                   Y = Off div 5,
                   case maps:get({X, Y}, State) of
                       $# -> Acc + (1 bsl Off);
                       _ -> Acc
                   end
                end,
                0,
                lists:seq(0, 24)).

game_of_life(Map) ->
    maps:from_list([{{X, Y}, next({X, Y}, Map)}
                    || X <- lists:seq(0, 4), Y <- lists:seq(0, 4)]).

%% A bug dies (becoming an empty space) unless there is exactly one
%% bug adjacent to it.  An empty space becomes infested with a bug if
%% exactly one or two bugs are adjacent to it.

next(Key, Map) ->
    case {maps:get(Key, Map), adjacent_bugs(Key, Map)} of
        {$#, N} when N =/= 1 ->
            $.;
        {$., 1} ->
            $#;
        {$., 2} ->
            $#;
        {C, _} ->
            C
    end.

adjacent_bugs({X, Y}, Map) ->
    N = maps:get({X, Y - 1}, Map, $.),
    E = maps:get({X + 1, Y}, Map, $.),
    S = maps:get({X, Y + 1}, Map, $.),
    W = maps:get({X - 1, Y}, Map, $.),
    lists:foldl(fun ($#, Acc) ->
                        Acc + 1;
                    (_, Acc) ->
                        Acc
                end,
                0,
                [N, E, S, W]).

%% ------------------------------------------------------------
%% Part 2: Each 5x5 grid is a cell in a bigger 5x5 grid, and each
%% center tile, is a grid in a smaller 5x5 grid.
%% ------------------------------------------------------------

part2(Binary, Repeats) ->
    Map = parse_part2(Binary),
    maps:fold(fun (_, $#, Acc) ->
                      Acc + 1;
                  (_, _, Acc) ->
                      Acc
              end,
              0,
              lists:foldl(fun game_of_life2/2, Map, lists:seq(1, Repeats))).

game_of_life2(_, Map) ->
    %% At what depths do we have bugs?
    {Min, Max} = get_depth_range(Map),

    %% Compute the next state by folding over all possible depths (Min -
    %% 1, Max + 1). Note that empty cells are filtered out (this is a
    %% rather significant performance optimization).
    maps:from_list([{{D, X, Y}, $#}
                    || D <- lists:seq(Min - 1, Max + 1),
                       X <- lists:seq(0, 4),
                       Y <- lists:seq(0, 4),
                       not ((X == 2) and (Y == 2)),              % Skip center tile
                       next2({D, X, Y}, Map) =:= $#]).           % Only keep bugs

get_depth_range(Map) ->
    maps:fold(fun({D, _, _}, _, {Min, Max}) -> {min(Min, D), max(Max, D)} end, {0, 0}, Map).

next2(Key, Map) ->
    case {maps:get(Key, Map, $.), adjacent_bugs2(Key, Map)} of
        {$#, N} when N =/= 1 ->
            $.;
        {$., 1} ->
            $#;
        {$., 2} ->
            $#;
        {C, _} ->
            C
    end.

adjacent_bugs2(Key, Map) ->
    AdjTiles = adjacent_tiles(Key),
    lists:foldl(fun(Tile, Acc) ->
                   Acc
                   + case maps:get(Tile, Map, $.) of
                         $# -> 1;
                         $. -> 0
                     end
                end,
                0,
                AdjTiles).

%% The rules of adjacency for part 2 were rather convoluted, and since
%% we only need to care about a 5x5 grid, I thought it easiest to just
%% hardcode them.

%%      |     |         |     |
%%   1  |  2  |    3    |  4  |  5
%%      |     |         |     |
%% -----+-----+---------+-----+-----
%%      |     |         |     |
%%   6  |  7  |    8    |  9  |  10
%%      |     |         |     |
%% -----+-----+---------+-----+-----
%%      |     |A|B|C|D|E|     |
%%      |     |-+-+-+-+-|     |
%%      |     |F|G|H|I|J|     |
%%      |     |-+-+-+-+-|     |
%%  11  | 12  |K|L|?|N|O|  14 |  15
%%      |     |-+-+-+-+-|     |
%%      |     |P|Q|R|S|T|     |
%%      |     |-+-+-+-+-|     |
%%      |     |U|V|W|X|Y|     |
%% -----+-----+---------+-----+-----
%%      |     |         |     |
%%  16  | 17  |    18   |  19 |  20
%%      |     |         |     |
%% -----+-----+---------+-----+-----
%%      |     |         |     |
%%  21  | 22  |    23   |  24 |  25
%%      |     |         |     |

%% =====================
%% ===== [ ROW 1 ] =====
%% =====================

%% Tile A is adjacent to 8, 12, B, F
adjacent_tiles({D, 0, 0}) ->
    [{D - 1, 2, 1},   % 8
     {D - 1, 1, 2},   % 12
     {D, 1, 0},   % B
     {D, 0, 1}];  % F
%% Tile B is adjacent to 8, A, C, G
adjacent_tiles({D, 1, 0}) ->
    [{D - 1, 2, 1},  % 8
     {D, 0, 0},  % A
     {D, 2, 0},  % C
     {D, 1, 1}]; % G
%% Tile C is adjacent to 8, B, D, H
adjacent_tiles({D, 2, 0}) ->
    [{D - 1, 2, 1},  % 8
     {D, 1, 0},  % B
     {D, 3, 0},  % D
     {D, 2, 1}];  % H
%% Tile D is adjacent to 8, C, E, I
adjacent_tiles({D, 3, 0}) ->
    [{D - 1, 2, 1},  % 8
     {D, 2, 0},  % C
     {D, 4, 0},  % E
     {D, 3, 1}]; % I
%% Tile E is adjacent to 8, 14, D, J
adjacent_tiles({D, 4, 0}) ->
    [{D - 1, 2, 1},  % 8
     {D - 1, 3, 2},  % 14
     {D, 3, 0},  % D
     {D, 4, 1}]; % J
%% =====================
%% ===== [ ROW 2 ] =====
%% =====================
%% Tile F is adjacent to 12, A, G, K
adjacent_tiles({D, 0, 1}) ->
    [{D - 1, 1, 2},  % 12
     {D, 0, 0},  % A
     {D, 1, 1},  % G
     {D, 0, 2}]; % K
%% Tile G is adjacent to B, F, H, L
adjacent_tiles({D, 1, 1}) ->
    [{D, 1, 0},  % B
     {D, 0, 1},  % F
     {D, 2, 1},  % H
     {D, 1, 2}]; % L
%% Tile H is adjacent to C, G, I, 1, 2, 3, 4, 5
adjacent_tiles({D, 2, 1}) ->
    [{D, 2, 0},  % C
     {D, 1, 1},  % G
     {D, 3, 1},  % I
     {D + 1, 0, 0},  % 1
     {D + 1, 1, 0},  % 2
     {D + 1, 2, 0},  % 3
     {D + 1, 3, 0},  % 4
     {D + 1, 4, 0}]; % 5
%% Tile I is adjacent to D, H, J, N
adjacent_tiles({D, 3, 1}) ->
    [{D, 3, 0},  % D
     {D, 2, 1},  % H
     {D, 4, 1},  % J
     {D, 3, 2}]; % N
%% Tile J is adjacent to E, I, 14, O
adjacent_tiles({D, 4, 1}) ->
    [{D, 4, 0},  % E
     {D, 3, 1},  % I
     {D - 1, 3, 2},  % 14
     {D, 4, 2}]; % O
%% =====================
%% ===== [ ROW 3 ] =====
%% =====================
%% Tile K is adjacent to F, 12, L, P
adjacent_tiles({D, 0, 2}) ->
    [{D, 0, 1},  % F
     {D - 1, 1, 2},  % 12
     {D, 1, 2},  % L
     {D, 0, 3}]; % P
%% Tile L is adjacent to G, K, 1, 6, 11, 16, 21, Q
adjacent_tiles({D, 1, 2}) ->
    [{D, 1, 1},  % G
     {D, 0, 2},  % K
     {D + 1, 0, 0},  % 1
     {D + 1, 0, 1},  % 6
     {D + 1, 0, 2},  % 11
     {D + 1, 0, 3},  % 16
     {D + 1, 0, 4},  % 21
     {D, 1, 3}];  % Q
%% Tile M is the recursive space
%% Tile N is adjacent to I, 5, 10, 15, 20, 25, O, S
adjacent_tiles({D, 3, 2}) ->
    [{D, 3, 1},  % I
     {D + 1, 4, 0},  % 5
     {D + 1, 4, 1},  % 10
     {D + 1, 4, 2},  % 15
     {D + 1, 4, 3},  % 20
     {D + 1, 4, 4},  % 25
     {D, 4, 2},  % O
     {D, 3, 3}]; % S
%% Tile O is adjacent to J, N, 14, T
adjacent_tiles({D, 4, 2}) ->
    [{D, 4, 1},  % J
     {D, 3, 2},  % N
     {D - 1, 3, 2},  % 14
     {D, 4, 3}]; % T
%% =====================
%% ===== [ ROW 4 ] =====
%% =====================
%% Tile P is adjacent to 12, K, Q, U
adjacent_tiles({D, 0, 3}) ->
    [{D - 1, 1, 2},  % 12
     {D, 0, 2},  % K
     {D, 1, 3},  % Q
     {D, 0, 4}]; % U
%% Tile Q is adjacent to L, P, R, V
adjacent_tiles({D, 1, 3}) ->
    [{D, 1, 2},  % L
     {D, 0, 3},  % P
     {D, 2, 3},  % R
     {D, 1, 4}]; % V
%% Tile R is adjacent to 21, 22, 23, 24, 25, Q, S, W
adjacent_tiles({D, 2, 3}) ->
    [{D + 1, 0, 4},  % 21
     {D + 1, 1, 4},  % 22
     {D + 1, 2, 4},  % 23
     {D + 1, 3, 4},  % 24
     {D + 1, 4, 4},  % 25
     {D, 1, 3},  % Q
     {D, 3, 3},  % S
     {D, 2, 4}]; % W
%% Tile S is adjacent to N, R, T, X
adjacent_tiles({D, 3, 3}) ->
    [{D, 3, 2},  % N
     {D, 2, 3},  % R
     {D, 4, 3},  % T
     {D, 3, 4}]; % X
%% Tile T is adjacent to O, S, 14, Y
adjacent_tiles({D, 4, 3}) ->
    [{D, 4, 2},  % O
     {D, 3, 3},  % S
     {D - 1, 3, 2},  % 14
     {D, 4, 4}]; % Y
%% =====================
%% ===== [ ROW 5 ] =====
%% =====================
%% Tile U is adjacent to 12, P, V, 18
adjacent_tiles({D, 0, 4}) ->
    [{D - 1, 1, 2},  % 12
     {D, 0, 3},  % P
     {D, 1, 4},  % V
     {D - 1, 2, 3}]; % 18
%% Tile V is adjacent to Q, U, W, 18
adjacent_tiles({D, 1, 4}) ->
    [{D, 1, 3},  % Q
     {D, 0, 4},  % U
     {D, 2, 4},  % W
     {D - 1, 2, 3}]; % 18
%% Tile W is adjacent to R, V, X, 18
adjacent_tiles({D, 2, 4}) ->
    [{D, 2, 3},  % R
     {D, 1, 4},  % V
     {D, 3, 4},  % X
     {D - 1, 2, 3}]; % 18
%% Tile X is adjacent to S, W, Y, 18
adjacent_tiles({D, 3, 4}) ->
    [{D, 3, 3},  % S
     {D, 2, 4},  % W
     {D, 4, 4},  % Y
     {D - 1, 2, 3}]; % 18
%% Tile Y is adjacent to T, X, 14, 18
adjacent_tiles({D, 4, 4}) ->
    [{D, 4, 3},  % T
     {D, 3, 4},  % X
     {D - 1, 3, 2},  % 14
     {D - 1, 2, 3}]. % 18

%% ============================================================
%% Parsing and tests
%% ============================================================

parse_part1(Bin) ->
    lists:foldl(fun({X, Y} = Key, Acc) ->
                   Off = Y * 5 + X,
                   maps:put(Key, binary:at(Bin, Off), Acc)
                end,
                #{},
                [{X, Y} || X <- lists:seq(0, 4), Y <- lists:seq(0, 4)]).

parse_part2(Bin) ->
    lists:foldl(fun({X, Y}, Acc) ->
                   Off = Y * 5 + X,
                   maps:put({0, X, Y}, binary:at(Bin, Off), Acc)
                end,
                #{},
                [{X, Y} || X <- lists:seq(0, 4), Y <- lists:seq(0, 4)]).
