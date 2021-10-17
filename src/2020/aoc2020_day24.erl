%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(aoc2020_day24).

-behavior(aoc_puzzle).

-export([ parse/1
        , solve1/1
        , solve2/1
        , info/0
        ]).

-include("aoc_puzzle.hrl").

%% This function is used in a call to lists:foldl/2, but does not
%% need its first argument.
-hank([{unnecessary_function_arguments, [{do_one_iter, 2, 1}]}]).

%%------------------------------------------------------------------------------
%% @doc info/0
%% Returns info about this puzzle.
%% @end
%%------------------------------------------------------------------------------
-spec info() -> aoc_puzzle().
info() ->
  #aoc_puzzle{ module = ?MODULE
             , year = 2020
             , day = 24
             , name = "Lobby Layout"
             , expected = {300, 3466}
             , has_input_file = true
             }.

%%==============================================================================
%% Types
%%==============================================================================
-type coord() :: { X :: integer()
                 , Y :: integer()
                 , Z :: integer()
                 }.
-type hexgrid() :: #{ coord() => black }.
-type input_type() :: hexgrid().

-type result1_type() :: integer().
-type result2_type() :: result1_type().

%%------------------------------------------------------------------------------
%% @doc parse/1
%% Parses input file.
%% @end
%%------------------------------------------------------------------------------
-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  flip_tiles(string:tokens(binary_to_list(Input), "\n\r")).

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(Input :: input_type()) -> result1_type().
solve1(Tiles) ->
  maps:size(Tiles).

%%------------------------------------------------------------------------------
%% @doc solve2/1
%% Solves part 2. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve2(Tiles :: input_type()) -> result2_type().
solve2(Tiles) ->
  N = 100,
  Final = lists:foldl(fun do_one_iter/2, Tiles, lists:seq(1, N)),
  maps:size(Final).

%%==============================================================================
%% Helpers
%%==============================================================================

%% Perform the initial tile flipping.
-spec flip_tiles(Input :: [string()]) -> hexgrid().
flip_tiles(Input) ->
  lists:foldl(
    fun(Line, Acc) ->
        FinalCoord =
          fold_coords(
            fun(Dir, {X, Y, Z}) ->
                %% Follow the conventions from
                %% https://www.redblobgames.com/grids/hexagons/#neighbors
                %% Note: sum of all the +1/-1 must be zero.
                case Dir of
                  "ne" -> {X + 1, Y,     Z - 1};
                  "e"  -> {X + 1, Y - 1, Z    };
                  "se" -> {X,     Y - 1, Z + 1};
                  "sw" -> {X - 1, Y,     Z + 1};
                  "w"  -> {X - 1, Y + 1, Z    };
                  "nw" -> {X,     Y + 1, Z - 1}
                end
            end, {0, 0, 0}, Line),

        %% Only store black keys, all other tiles are white
        case maps:is_key(FinalCoord, Acc) of
          true -> maps:remove(FinalCoord, Acc);
          false -> maps:put(FinalCoord, black, Acc)
        end

    end, #{}, Input).

fold_coords(_Fun, State, []) ->
  State;
fold_coords(Fun, State, [A, B|Rest]) when ([A, B] =:= "ne") orelse
                                          ([A, B] =:= "nw") orelse
                                          ([A, B] =:= "sw") orelse
                                          ([A, B] =:= "se") ->
  fold_coords(Fun, Fun([A, B], State), Rest);
fold_coords(Fun, State, [A|Rest]) when ([A] =:= "e") orelse
                                       ([A] =:= "w") ->
  fold_coords(Fun, Fun([A], State), Rest).

%% ======================================================================
%% Part 2 iteration code
%% ======================================================================

%% Represent coordinates using maps-as-sets.
get_coords_to_check(Tiles) ->
  maps:fold(
    fun(Coord, _, Acc) ->
        maps:merge(
          Acc,
          maps:merge(#{Coord => ignore}, neighbors(Coord)))
    end, #{}, Tiles).

do_one_iter(_N, Tiles) ->
  maps:fold(
    fun(Coord, ignore, Acc) ->
        Neighbors = neighbors(Coord),
        IsBlack = maps:is_key(Coord, Tiles),
        case {IsBlack, count_black_neighbors(Neighbors, Tiles)} of
          {true, 0} -> maps:remove(Coord, Acc);
          {true, NB} when NB > 2 -> maps:remove(Coord, Acc);
          {false, 2} -> maps:put(Coord, black, Acc);
          _ -> Acc
        end
    end, Tiles, get_coords_to_check(Tiles)).

count_black_neighbors(Neighbors, Tiles) ->
  maps:fold(
    fun(Coord, ignore, N) ->
        case maps:get(Coord, Tiles, white) of
          black -> N + 1;
          _ -> N
        end
    end, 0, Neighbors).

neighbors({X, Y, Z} = Coord) ->
  %% Cache neighbor lists in the process dictionary (these are static;
  %% the neighbors of a given tile will always be the same).
  case get(Coord) of
    undefined ->
      Nbrs = maps:from_list(
               [{{X + Dx, Y + Dy, Z + Dz}, ignore}
                || Dx <- [-1, 0, 1],
                   Dy <- [-1, 0, 1],
                   Dz <- [-1, 0, 1],
                   {Dy, Dx, Dz} =/= {0, 0, 0},
                   Dy + Dx + Dz == 0]),
      put(Coord, Nbrs),
      Nbrs;
    Nbrs -> Nbrs
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
