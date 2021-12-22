-module(aoc2021_day22).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-compile([export_all, nowarn_export_all]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 22,
                name = "Reactor Reboot",
                expected = {601104, 0},
                has_input_file = true}.

-type input_type() :: any().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    F = fun list_to_integer/1,
    lists:map(fun(B) ->
                 [OnOff, XMin, XMax, YMin, YMax, ZMin, ZMax] =
                     string:tokens(binary_to_list(B), " =..,xyz"),
                 {list_to_atom(OnOff), F(XMin), F(XMax), F(YMin), F(YMax), F(ZMin), F(ZMax)}
              end,
              binary:split(Binary, <<"\n">>, [trim_all, global])).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    InitCubes =
        lists:filter(fun({_, XMin, XMax, YMin, YMax, ZMin, ZMax}) ->
                        XMin >= -50
                        andalso XMax =< 50
                        andalso YMin >= -50
                        andalso YMax =< 50
                        andalso ZMin >= -50
                        andalso ZMax =< 50
                     end,
                     Input),
    maps:size(switch_cubes(InitCubes, #{})).

-spec solve2(Input :: input_type()) -> result_type().
solve2(_Input) ->
    0.
    % Ranges = sweep_x(Input, []),
    % Squares = sweep_y(Ranges, Input, []),
    % _Cuboids = sweep_z(Squares, Input, []).


% sweep_y([{XMin, XMax} | Rest], Instr, SquaresOut) ->
%     Squares = lists:foldl(fun({_, XMin0, XMax0, YMin, YMax, _, _}, Acc) ->
%                    case overlapping_range({XMin, XMax}, {XMin0, XMax0}) of
%                        false -> Acc;
%                        {OverlapXMin, OverlapXMax} -> [{OverlapXMin, OverlapXMax, YMin, YMax} | Acc]
%                    end
%                 end,
%                 [],
%                 Instr).

switch_cubes([], Map) -> Map;
switch_cubes([{OnOff, XMin, XMax, YMin, YMax, ZMin, ZMax} | Rest], Map) ->
    Coords =
        [{X, Y, Z}
         || X <- lists:seq(XMin, XMax), Y <- lists:seq(YMin, YMax), Z <- lists:seq(ZMin, ZMax)],
    switch_cubes(Rest,
                 lists:foldl(fun(Coord, Acc) ->
                                case OnOff of
                                    on -> maps:put(Coord, true, Acc);
                                    off -> maps:remove(Coord, Acc)
                                end
                             end,
                             Map,
                             Coords)).

%         {_, XMin1, XMax1, YMin1, YMax1, ZMin1, ZMax1}) ->
%     OverlapX = overlapping_range({XMin0, XMax0}, {XMin1, XMax1}),
%     OverlapY = overlapping_range({YMin0, YMax0}, {YMin1, YMax1}),
%     OverlapZ = overlapping_range({ZMin0, ZMax0}, {ZMin1, ZMax1}),
%     if not OverlapX orelse not OverlapY orelse not OverlapZ ->
%            % Boxes only overlap if all the X, Y, and Z coordinates overlap.
%            false;
%        true ->
%            {OverlapXMin, OverlapXMax} = OverlapX,
%            {OverlapYMin, OverlapYMax} = OverlapY,
%            {OverlapZMin, OverlapZMax} = OverlapZ,

%            _OverlapCuboid =
%                {OverlapXMin, OverlapXMax, OverlapYMin, OverlapYMax, OverlapZMin, OverlapZMax},

%             NonOverlapXAbove = {OverlapXMax, max(XMax0, XMax1)},
%             NonOverlapYAbove = {OverlapYMax, max(YMax0, YMax1)},
%             NonOverlapZAbove = {OverlapZMax, max(ZMax0, ZMax1)},

%             NonOverlapXBelow = {OverlapXMin, min(XMin0, XMin1)},
%             NonOverlapYBelow = {OverlapYMin, min(YMin0, YMin1)},
%             NonOverlapZBelow = {OverlapZMin, min(ZMin0, ZMin1)}

%     end.

overlapping_range({A, B}, {C, D}) ->
    Min = max(A, C),
    Max = min(B, D),
    case Min > Max of
        true ->
            %% Ranges do not overlap
            false;
        false ->
            {Min, Max}
    end.

%% Tests
-ifdef(TEST).

overlap_test() ->
    %% A: ..[..]....
    %% B: ....[..]..
    ?assertEqual({5, 10}, overlapping_range({-10, 10}, {5, 15})),

    %% A: ..[......]..
    %% B: ....[..]....
    ?assertEqual({-10, 12}, overlapping_range({-10, 12}, {-12, 15})),

    %% A: ..[..]........
    %% B: ........[..]..
    ?assertEqual(false, overlapping_range({-10, 5}, {10, 15})).

ex1_test() ->
    Binary =
        <<"on x=10..12,y=10..12,z=10..12\non x=11..13,y=11..13,z=11..13\noff "
          "x=9..11,y=9..11,z=9..11\non x=10..10,y=10..10,z=10..10">>,
    X = solve2(parse(Binary)),
    ?debugFmt("~n~p", [X]).

-endif.
