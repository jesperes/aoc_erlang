-module(aoc2021_day03).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).
-include_lib("eunit/include/eunit.hrl").

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 3,
                name = "Binary Diagnostics",
                expected = {3549854, 3765399},
                has_input_file = true}.

-type input_type() :: [binary()].
-type result_type() :: any().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    binary:split(Binary, <<"\n">>, [trim_all, global]).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    Map =
        lists:foldl(
          fun(Bin, Map) ->
                  update_freq_map(Bin, Map, 0)
          end, #{}, Input),

    L = lists:sort(maps:to_list(Map)),

    {Gamma, Epsilon} =
        lists:foldl(
          fun({_Pos, {Num0, Num1}}, {Gamma, Epsilon}) ->
                  case Num0 > Num1 of
                      true -> {<<Gamma/binary, $0>>, <<Epsilon/binary, $1>>};
                      false -> {<<Gamma/binary, $1>>, <<Epsilon/binary, $0>>}
                  end
          end, {<<>>, <<>>}, L),

    binary_to_integer(Gamma, 2) *
        binary_to_integer(Epsilon, 2).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    Oxygen = filter_nums(
               Input, 0,
               fun(Elem, Num0, Num1) ->
                       if Num0 > Num1 -> Elem =:= $0;
                          Num1 >= Num0 -> Elem =:= $1
                       end
               end),
    CO2 = filter_nums(
            Input, 0,
            fun(Elem, Num0, Num1) ->
                    if Num0 =< Num1 -> Elem =:= $0;
                       Num1 < Num0 -> Elem =:= $1
                    end
            end),
    binary_to_integer(Oxygen, 2) * binary_to_integer(CO2, 2).

%% Part 1

update_freq_map(<<>>, Map, _) ->
    Map;
update_freq_map(<<N, Rest/binary>>, Map, Pos) ->
    update_freq_map(
      Rest,
      maps:update_with(
        Pos,
        fun(Old) ->
                update_freq(N, Old)
        end, update_freq(N, {0, 0}), Map),
      Pos + 1).

update_freq($0, {Num0, Num1}) ->
    {Num0 + 1, Num1};
update_freq($1, {Num0, Num1}) ->
    {Num0, Num1 + 1}.

%% Part 2

filter_nums([Elem], _, _) ->
    Elem;
filter_nums(L, Pos, FilterFun) ->
    NewList = filter_nums_at_pos(L, Pos, FilterFun),
    filter_nums(NewList, Pos + 1, FilterFun).

filter_nums_at_pos(List, Pos, FilterFun) ->
    %% Compute the number of 0s and 1s at this position. This needs to
    %% be done anew for each pass (position), since the frequency
    %% distribution will change as we filter out numbers.
    {Num0, Num1} =
        lists:foldl(
          fun(Bin, Acc) ->
                  update_freq(binary:at(Bin, Pos), Acc)
          end, {0, 0}, List),

    lists:filter(
      fun(Elem) ->
              FilterFun(binary:at(Elem, Pos), Num0, Num1)
      end, List).

%% Tests
-ifdef(TEST).

ex1_input() ->
    [<<"00100">>,
     <<"11110">>,
     <<"10110">>,
     <<"10111">>,
     <<"10101">>,
     <<"01111">>,
     <<"00111">>,
     <<"11100">>,
     <<"10000">>,
     <<"11001">>,
     <<"00010">>,
     <<"01010">>].

ex1_test() ->
    ?assertEqual(198, solve1(ex1_input())).

ex2_test() ->
    ?assertEqual(230, solve2(ex1_input())).

-endif.
