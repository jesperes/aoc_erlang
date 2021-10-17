-module(aoc2015_day16).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 16,
                name = "Aunt Sue",
                expected = {213, 323},
                has_input_file = true}.

-type input_type() :: map().
-type result1_type() :: any().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    maps:from_list(
        lists:map(fun(Line) ->
                     [_, Num | Rest] = string:tokens(Line, ": ,"),
                     {list_to_integer(Num), list_to_dict(Rest)}
                  end,
                  string:tokens(binary_to_list(Input), "\r\n"))).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    find(Input, fun matches1/3).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    find(Input, fun matches2/3).

mfcsam_output() ->
    #{children => 3,
      cats => 7,
      samoyeds => 2,
      pomeranians => 3,
      akitas => 0,
      vizslas => 0,
      goldfish => 5,
      trees => 3,
      cars => 2,
      perfumes => 1}.

matches1(cats, AuntVal, MfcSamValue) ->
    AuntVal == MfcSamValue;
matches1(trees, AuntVal, MfcSamValue) ->
    AuntVal == MfcSamValue;
matches1(pomeranians, AuntVal, MfcSamValue) ->
    AuntVal == MfcSamValue;
matches1(goldfish, AuntVal, MfcSamValue) ->
    AuntVal == MfcSamValue;
matches1(_, AuntVal, MfcSamValue) ->
    AuntVal == MfcSamValue.

matches2(cats, AuntVal, MfcSamValue) ->
    AuntVal > MfcSamValue;
matches2(trees, AuntVal, MfcSamValue) ->
    AuntVal > MfcSamValue;
matches2(pomeranians, AuntVal, MfcSamValue) ->
    AuntVal < MfcSamValue;
matches2(goldfish, AuntVal, MfcSamValue) ->
    AuntVal < MfcSamValue;
matches2(_, AuntVal, MfcSamValue) ->
    AuntVal == MfcSamValue.

find(Map, MatchFun) ->
    MfcSam = mfcsam_output(),
    MfcSamKeys =
        sets:from_list(
            maps:keys(MfcSam)),

    ResultMap =
        maps:filter(fun(_, M) ->
                       Keys1 =
                           sets:from_list(
                               maps:keys(M)),
                       CommonKeys = sets:intersection(Keys1, MfcSamKeys),
                       lists:all(fun(K) ->
                                    V1 = maps:get(K, M),
                                    V2 = maps:get(K, MfcSam),
                                    MatchFun(K, V1, V2)
                                 end,
                                 sets:to_list(CommonKeys))
                    end,
                    Map),

    [{X, _} | _] = maps:to_list(ResultMap),
    X.

list_to_dict([]) ->
    #{};
list_to_dict([Key, Value | Rest]) ->
    maps:put(list_to_atom(Key), list_to_integer(Value), list_to_dict(Rest)).
