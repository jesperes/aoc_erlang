%% Stolen/borrowed from
%% https://github.com/Andrew-William-Smith/advent-of-code/blob/2021-erlang/src/aoc2021_day22.erl
-module(aoc2021_day22).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 22,
                name = "Reactor Reboot",
                expected = {601104, 1262883317822267},
                has_input_file = true}.

-type input_type() :: integer().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    F = fun list_to_integer/1,
    lists:map(fun(B) ->
                 [OnOff, XMin, XMax, YMin, YMax, ZMin, ZMax] =
                     string:tokens(binary_to_list(B), " =..,xyz"),
                 {list_to_atom(OnOff),
                  {{F(XMin), F(XMax) + 1}, {F(YMin), F(YMax) + 1}, {F(ZMin), F(ZMax) + 1}}}
              end,
              binary:split(Binary, <<"\n">>, [trim_all, global])).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    BoundCuboid = {{-50, 50}, {-50, 50}, {-50, 50}},
    InBounds = [Step || {_, C} = Step <- Input, cuboid_difference(C, BoundCuboid) /= [C]],
    lists:sum(
        lists:map(fun cuboid_volume/1, reboot_reactor([], InBounds))).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    lists:sum(
        lists:map(fun cuboid_volume/1, reboot_reactor([], Input))).

cuboid_difference({{XL1, XH1}, {YL1, YH1}, {ZL1, ZH1}} = A,
                  {{XL2, XH2}, {YL2, YH2}, {ZL2, ZH2}})
    when XL1 > XH2
         orelse XL2 > XH1
         orelse YL1 > YH2
         orelse YL2 > YH1
         orelse ZL1 > ZH2
         orelse ZL2 > ZH1 ->
    [A];
cuboid_difference({{XL1, XH1} = X1, {YL1, YH1}, {ZL1, ZH1} = Z1},
                  {{XL2, XH2}, {YL2, YH2}, {ZL2, ZH2}}) ->
    Top = ?_if(YL2 > YL1, [{X1, {YL1, YL2}, Z1}], []),
    Bottom = ?_if(YH1 > YH2, [{X1, {YH2, YH1}, Z1}], []),
    YBounds = {max(YL1, YL2), min(YH1, YH2)},
    Left = ?_if(XL2 > XL1, [{{XL1, XL2}, YBounds, Z1}], []),
    Right = ?_if(XH1 > XH2, [{{XH2, XH1}, YBounds, Z1}], []),
    XBounds = {max(XL1, XL2), min(XH1, XH2)},
    Near = ?_if(ZL2 > ZL1, [{XBounds, YBounds, {ZL1, ZL2}}], []),
    Far = ?_if(ZH1 > ZH2, [{XBounds, YBounds, {ZH2, ZH1}}], []),
    Top ++ Bottom ++ Left ++ Right ++ Near ++ Far.

reboot_reactor(Cuboids, []) ->
    Cuboids;
reboot_reactor(Cuboids, [{Instruction, NewCuboid} | RestStep]) ->
    Difference = lists:flatten([cuboid_difference(C, NewCuboid) || C <- Cuboids]),
    NextCuboids =
        case Instruction of
            on ->
                [NewCuboid | Difference];
            off ->
                Difference
        end,
    reboot_reactor(NextCuboids, RestStep).

cuboid_volume({{XLo, XHi}, {YLo, YHi}, {ZLo, ZHi}}) ->
    (XHi - XLo) * (YHi - YLo) * (ZHi - ZLo).
