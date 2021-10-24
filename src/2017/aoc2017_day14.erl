-module(aoc2017_day14).

-behavior(aoc_puzzle).
-include_lib("eunit/include/eunit.hrl").

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 14,
                name = "Disk Defragmentation",
                expected = {8230, 1103},
                has_input_file = false}.

-type input_type() :: [string()].
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(_Binary) ->
    Input = "hfdlxzhv",
    lists:map(
      fun(N) ->
              Str = lists:flatten(io_lib:format("~s-~w", [Input, N])),
              str_to_knot_hash_bits(Str)
      end, lists:seq(0, 127)).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Bits) ->
    Ones = lists:filter(
             fun(C) -> C =:= $1 end,
             lists:flatten(Bits)),
    length(Ones).

str_to_knot_hash_bits(Str) ->
    KnotHash = aoc2017_day10:knot_hash(Str),
    Bits = integer_to_list(list_to_integer(KnotHash, 16), 2),
    string:right(Bits, 128, $0).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Lines) ->
    Set = bits_to_coord_set(Lines),
    count_regions(Set).

bits_to_coord_set(Lines) ->
    {_, Set} =
        lists:foldl(
          fun(Line, {Y, Coords}) ->
                  CoordsOut = fold_line(Line, Y, Coords),
                  {Y + 1, CoordsOut}
          end, {0, gb_sets:new()}, Lines),
    Set.

fold_line(Line, Y, Coords) ->
    {_, CoordsOut} =
        lists:foldl(
          fun($1, {X, Acc}) ->
                  {X + 1, gb_sets:add_element({X, Y}, Acc)};
             (_, {X, Acc}) ->
                  {X + 1, Acc}
          end, {0, Coords}, Line),
    CoordsOut.

count_regions(Set) ->
    count_regions(Set, 1).

count_regions(Set, Num) ->
    {First, Set0} = gb_sets:take_smallest(Set),
    Set1 = fill_at(First, Num, Set0),
    case gb_sets:is_empty(Set1) of
        true -> Num;
        false -> count_regions(Set1, Num + 1)
    end.

fill_at({X, Y}, Num, Set) ->
    Neighbors =
        [ Nbr || Nbr <- [ {X - 1, Y},
                          {X, Y - 1},
                          {X + 1, Y},
                          {X, Y + 1}
                        ],
                 gb_sets:is_element(Nbr, Set)
        ],
    lists:foldl(
      fun(Nbr, Acc) ->
              fill_at(Nbr, Num, gb_sets:del_element(Nbr, Acc))
      end, Set, Neighbors).
