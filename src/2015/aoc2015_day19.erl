-module(aoc2015_day19).

-include("aoc_puzzle.hrl").

-export([parse/1, solve1/1, solve2/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 19,
                name = "Medicine for Rudolph",
                expected = {576, 207},
                has_input_file = false}.

-type input_type() :: binary().
-type result1_type() :: any().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    Input.

-spec solve1(Input :: input_type()) -> result1_type().
solve1(_Input) ->
    part1().

-spec solve2(Input :: input_type()) -> result2_type().
solve2(_Input) ->
    part2().

-define(INPUT,
        <<"ORnPBPMgArCaCaCaSiThCaCaSiThCaCaPBSiRnFArRnFArCaCaSiThCaCaSiThCaCaCa"
          "CaCaCaSiRnFYFArSiRnMgArCaSiRnPTiTiBFYPBFArSiRnCaSiRnTiRnFArSiAlArPTi"
          "BPTiRnCaSiAlArCaPTiTiBPMgYFArPTiRnFArSiRnCaCaFArRnCaFArCaSiRnSiRnMgA"
          "rFYCaSiRnMgArCaCaSiThPRnFArPBCaSiRnMgArCaCaSiThCaSiRnTiMgArFArSiThSi"
          "ThCaCaSiRnMgArCaCaSiRnFArTiBPTiRnCaSiAlArCaPTiRnFArPBPBCaCaSiThCaPBS"
          "iThPRnFArSiThCaSiThCaSiThCaPTiBSiRnFYFArCaCaPRnFArPBCaCaPBSiRnTiRnFA"
          "rCaPRnFArSiRnCaCaCaSiThCaRnCaFArYCaSiRnFArBCaCaCaSiThFArPBFArCaSiRnF"
          "ArRnCaCaCaFArSiRnFArTiRnPMgArF">>).

-define(INPUT_RULES,
        [{<<"Al">>, <<"ThF">>}, {<<"Al">>, <<"ThRnFAr">>}, {<<"B">>, <<"BCa">>},
         {<<"B">>, <<"TiB">>}, {<<"B">>, <<"TiRnFAr">>}, {<<"Ca">>, <<"CaCa">>},
         {<<"Ca">>, <<"PB">>}, {<<"Ca">>, <<"PRnFAr">>}, {<<"Ca">>, <<"SiRnFYFAr">>},
         {<<"Ca">>, <<"SiRnMgAr">>}, {<<"Ca">>, <<"SiTh">>}, {<<"F">>, <<"CaF">>},
         {<<"F">>, <<"PMg">>}, {<<"F">>, <<"SiAl">>}, {<<"H">>, <<"CRnAlAr">>},
         {<<"H">>, <<"CRnFYFYFAr">>}, {<<"H">>, <<"CRnFYMgAr">>}, {<<"H">>, <<"CRnMgYFAr">>},
         {<<"H">>, <<"HCa">>}, {<<"H">>, <<"NRnFYFAr">>}, {<<"H">>, <<"NRnMgAr">>},
         {<<"H">>, <<"NTh">>}, {<<"H">>, <<"OB">>}, {<<"H">>, <<"ORnFAr">>}, {<<"Mg">>, <<"BF">>},
         {<<"Mg">>, <<"TiMg">>}, {<<"N">>, <<"CRnFAr">>}, {<<"N">>, <<"HSi">>},
         {<<"O">>, <<"CRnFYFAr">>}, {<<"O">>, <<"CRnMgAr">>}, {<<"O">>, <<"HP">>},
         {<<"O">>, <<"NRnFAr">>}, {<<"O">>, <<"OTi">>}, {<<"P">>, <<"CaP">>}, {<<"P">>, <<"PTi">>},
         {<<"P">>, <<"SiRnFAr">>}, {<<"Si">>, <<"CaSi">>}, {<<"Th">>, <<"ThCa">>},
         {<<"Ti">>, <<"BP">>}, {<<"Ti">>, <<"TiTi">>}, {<<"e">>, <<"HF">>}, {<<"e">>, <<"NAl">>},
         {<<"e">>, <<"OMg">>}]).

part1() ->
    length(get_all_replacements(?INPUT, ?INPUT_RULES)).

part2() ->
    %% This uses the trick described in
    %% https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju
    %% which describes how to compute the minimal number of steps
    %% needed without having to actually figure out which
    %% substitutions to make.
    lists:foldl(fun(C, Steps) ->
                   S0 = case is_lower(C) of
                            false -> Steps + 1;
                            true -> Steps
                        end,

                   case C of
                       $( -> S0 - 1;
                       $) -> S0 - 1;
                       $, -> S0 - 2;
                       _ -> S0
                   end
                end,
                -1,
                replace_paren(binary_to_list(?INPUT))).

replace_paren([]) ->
    [];
replace_paren([$A, $r | Rest]) ->
    [$( | replace_paren(Rest)];
replace_paren([$R, $n | Rest]) ->
    [$) | replace_paren(Rest)];
replace_paren([$Y | Rest]) ->
    [$, | replace_paren(Rest)];
replace_paren([X | Rest]) ->
    [X | replace_paren(Rest)].

is_lower(C) when (C >= $a) and (C =< $z) ->
    true;
is_lower(_) ->
    false.

get_all_replacements(Input, Rules) ->
    Repls = [apply_rule(Rule, Input) || Rule <- Rules],
    sets:to_list(
        sets:from_list(
            lists:flatten(Repls))).

apply_rule({F, T}, Input) ->
    [binary:replace(Input, F, T, [{scope, Pos}]) || Pos <- binary:matches(Input, F)].
