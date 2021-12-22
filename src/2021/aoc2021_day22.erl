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

-define(IS_INIT(XMin, XMax, YMin, YMax, ZMin, ZMax),
        XMin >= -50
        andalso XMax =< 50
        andalso YMin >= -50
        andalso YMax =< 50
        andalso ZMin >= -50
        andalso ZMax =< 50).

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    F = fun list_to_integer/1,
    lists:map(fun(B) ->
                 [OnOff, XMin, XMax, YMin, YMax, ZMin, ZMax] =
                     string:tokens(binary_to_list(B), " =..,xyz"),
                 {list_to_atom(OnOff), {F(XMin), F(XMax)}, {F(YMin), F(YMax)}, {F(ZMin), F(ZMax)}}
              end,
              binary:split(Binary, <<"\n">>, [trim_all, global])).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    count_cubes(do_solve(lists:filter(fun({_, {XMin, XMax}, {YMin, YMax}, {ZMin, ZMax}}) ->
                                         ?IS_INIT(XMin, XMax, YMin, YMax, ZMin, ZMax)
                                      end,
                                      Input))).

-spec solve2(Input :: input_type()) -> result_type().
solve2(_Input) ->
    0.

    % do_solve(Input).

count_cubes([]) ->
    0;
count_cubes([{off, _, _, _} | Rest]) ->
    count_cubes(Rest);
count_cubes([{on, {XMin, XMax}, {YMin, YMax}, {ZMin, ZMax}} | Rest]) ->
    (XMax - XMin) * (YMax - YMin) * (ZMax - ZMin) + count_cubes(Rest).

do_solve(Instrs) ->
    do_solve(tl(Instrs), [hd(Instrs)]).

do_solve([], NonOverlapping) ->
    NonOverlapping;
do_solve([A | Rest], NonOverlapping) ->
    ?assert(not has_overlapping_cuboids(NonOverlapping)),

    io:format(standard_error,
              "~nChecking ~p (~p remaining) (~p non-overlaps)~n",
              [A, length(Rest), length(NonOverlapping)]),
    %% Find all the cuboids in the non-overlapping list which overlap with A
    {OverlapWithA, NonOverlapWithA} =
        lists:partition(fun(B) -> are_cuboids_overlapping(A, B) end, NonOverlapping),

    io:format(standard_error, "-- overlapping: ~p~n", [OverlapWithA]),
    io:format(standard_error, "-- non-overlapping: ~p~n", [NonOverlapWithA]),

    %% Split the overlapping ones
    NewNonOverlaps =
        NonOverlapWithA
        ++ lists:foldl(fun(B, Acc) -> split_cuboids(A, B) ++ Acc end, [], OverlapWithA),

    io:format(standard_error, "Non-overlaps after splitting~n~p~n", [NewNonOverlaps]),

    do_solve(Rest, NewNonOverlaps).

% all ranges are inclusive

split_cuboids({OnOffA,
               {MinXA, MaxXA} = XRangeA,
               {MinYA, MaxYA} = YRangeA,
               {MinZA, MaxZA} = ZRangeA} =
                  A,
              {OnOffB,
               {MinXB, MaxXB} = XRangeB,
               {MinYB, MaxYB} = YRangeB,
               {MinZB, MaxZB} = ZRangeB} =
                  B) ->
    {OverlapXMin, OverlapXMax} = OverlapXRange = overlap(XRangeA, XRangeB),
    {OverlapYMin, OverlapYMax} = OverlapYRange = overlap(YRangeA, YRangeB),
    {OverlapZMin, OverlapZMax} = OverlapZRange = overlap(ZRangeA, ZRangeB),

    OverlapCuboid = {OnOffB, OverlapXRange, OverlapYRange, OverlapZRange},

    case is_valid_cuboid(OverlapCuboid) of
        false ->
            % If there is no overlap, just return the unmodified cuboids
            [A, B];
        true ->
            CX = if MinXA < MinXB andalso MaxXA < MaxXB ->
                        %% A: ..[.||]....
                        %% B: ....[||.]..
                        [{OnOffA, {MinXA, OverlapXMin - 1}, YRangeA, ZRangeA},
                         {OnOffB, {OverlapXMax + 1, MaxXB}, YRangeB, ZRangeB}];
                    MinXA < MinXB andalso MaxXA > MaxXB ->
                        %% A: ..[.||||.]..
                        %% B: ....[..]....
                        [{OnOffA, {MinXA, OverlapXMin - 1}, YRangeA, ZRangeA},
                         {OnOffA, {OverlapXMax + 1, MaxXB}, YRangeB, ZRangeB}];
                    true ->
                        [{OnOffA, {MinXB, OverlapXMin - 1}, YRangeA, ZRangeA},
                         {OnOffB, {OverlapXMax + 1, MaxXA}, YRangeB, ZRangeB}]
                 end,

            CY = if MinYA < OverlapYMin ->
                        [{OnOffA, OverlapXRange, {MinYA, OverlapYMin - 1}, ZRangeA},
                         {OnOffB, OverlapXRange, {OverlapYMax + 1, MaxYB}, ZRangeB}];
                    true ->
                        [{OnOffA, OverlapXRange, {MinYB, OverlapYMin - 1}, ZRangeA},
                         {OnOffB, OverlapXRange, {OverlapYMax + 1, MaxYA}, ZRangeB}]
                 end,

            CZ = if MinZA < OverlapZMin ->
                        [{OnOffA, OverlapXRange, OverlapYRange, {MinZA, OverlapZMin - 1}},
                         {OnOffB, OverlapXRange, OverlapYRange, {OverlapZMax + 1, MaxZB}}];
                    true ->
                        [{OnOffA, OverlapXRange, OverlapYRange, {MinZB, OverlapZMin - 1}},
                         {OnOffB, OverlapXRange, OverlapYRange, {OverlapZMax + 1, MaxZA}}]
                 end,

            lists:filter(fun is_valid_cuboid/1, [OverlapCuboid] ++ CX ++ CY ++ CZ)
    end.

overlap({A, B}, {C, D}) ->
    Min = max(A, C),
    Max = min(B, D),
    {Min, Max}.

is_valid_cuboid({_, XRange, YRange, ZRange}) ->
    is_valid_range(XRange) andalso is_valid_range(YRange) andalso is_valid_range(ZRange).

is_valid_range({A, B}) ->
    B >= A.

is_overlapping_range(RangeA, RangeB) ->
    is_valid_range(overlap(RangeA, RangeB)).

are_cuboids_overlapping({_, XRangeA, YRangeA, ZRangeA}, {_, XRangeB, YRangeB, ZRangeB}) ->
    is_overlapping_range(XRangeA, XRangeB)
    andalso is_overlapping_range(YRangeA, YRangeB)
    andalso is_overlapping_range(ZRangeA, ZRangeB).

has_overlapping_cuboids(List) ->
    lists:any(fun({A, B}) ->
                 case are_cuboids_overlapping(A, B) of
                     true ->
                         % io:format(standard_error, "Overlapping:~n~p~n~p~n", [A, B]),
                         true;
                     _ -> false
                 end
              end,
              [{A, B} || A <- List, B <- List, A < B]).

%% Tests
-ifdef(TEST).

overlap_test() ->
    %% A: ..[..]....
    %% B: ....[..]..
    ?assertEqual({5, 10}, overlap({-10, 10}, {5, 15})),

    %% A: ..[......]..
    %% B: ....[..]....
    ?assertEqual({-10, 12}, overlap({-10, 12}, {-12, 15})),

    %% A: ..[..]........
    %% B: ........[..]..
    ?assertEqual(false, is_valid_range(overlap({-10, 5}, {10, 15}))).

split_cuboids_test() ->
    Split = split_cuboids({a, {0, 10}, {0, 10}, {0, 10}}, {b, {5, 15}, {5, 15}, {5, 15}}),
    ?assertNot(has_overlapping_cuboids(Split)),
    io:format(standard_error, ">> ~p~n", [Split]).

ex1_test() ->
    Binary =
        <<"on x=10..12,y=10..12,z=10..12\non x=11..13,y=11..13,z=11..13\noff "
          "x=9..11,y=9..11,z=9..11\non x=10..10,y=10..10,z=10..10">>,
    X = solve2(parse(Binary)),
    ?debugFmt("~n~p", [X]).

solve_test() ->
    ?assertEqual(1115,
                 solve1([{on, {0, 10}, {0, 10}, {0, 10}}, {off, {5, 15}, {5, 15}, {5, 15}}])).

-endif.
