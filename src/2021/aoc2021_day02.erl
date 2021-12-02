-module(aoc2021_day02).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 2,
                name = "Dive!",
                expected = {1962940, 1813664422},
                has_input_file = true}.

-type input_type() :: [{binary(), pos_integer()}].
-type result_type() :: pos_integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    lists:map(fun(Line) ->
                      [Cmd, X] = binary:split(Line, <<" ">>),
                      {Cmd, binary_to_integer(X)}
              end, binary:split(Binary, <<"\n">>, [trim_all, global])).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    {Depth, Pos} =
        lists:foldl(
          fun({Cmd, X}, {Depth, Pos}) ->
                  case Cmd of
                      <<"forward">> -> {Depth, Pos + X};
                      <<"down">> ->    {Depth + X, Pos};
                      <<"up">> ->      {Depth - X, Pos}
                  end
          end, {0, 0}, Input),
    Depth * Pos.

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    {Depth, Pos, _Aim} =
        lists:foldl(
          fun({Cmd, X}, {Depth, Pos, Aim}) ->
                  case Cmd of
                      <<"forward">> -> {Depth + Aim * X, Pos + X, Aim};
                      <<"down">> ->    {Depth, Pos, Aim + X};
                      <<"up">> ->      {Depth, Pos, Aim - X}
                  end
          end, {0, 0, 0}, Input),
    Depth * Pos.
