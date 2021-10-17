-module(aoc2015_day12).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 12,
                name = "JSAbacusFramework.io",
                expected = {119433, 68466},
                has_input_file = true}.

-type input_type() :: jsone:json_object().
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    jsone:decode(Input).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    count(Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    count_nored(Input).

count(X) when is_number(X) ->
    X;
count(X) when is_binary(X) ->
    0;
count(X) when is_map(X) ->
    maps:fold(fun(_, V, Acc) -> count(V) + Acc end, 0, X);
count(X) when is_list(X) ->
    lists:foldl(fun(V, Acc) -> count(V) + Acc end, 0, X).

count_nored(X) when is_number(X) ->
    X;
count_nored(X) when is_binary(X) ->
    0;
count_nored(X) when is_map(X) ->
    {IsRed, Sum} =
        maps:fold(fun (_, <<"red">>, {_, _Acc}) ->
                          {true, 0};
                      (_, V, {IsRed, Acc}) ->
                          {IsRed, Acc + count_nored(V)}
                  end,
                  {false, 0},
                  X),
    case IsRed of
        true ->
            0;
        false ->
            Sum
    end;
count_nored(X) when is_list(X) ->
    lists:foldl(fun(V, Acc) -> count_nored(V) + Acc end, 0, X).
