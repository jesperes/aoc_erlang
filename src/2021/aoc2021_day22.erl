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
    io:format(standard_error, "~p~n", [Input]),
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
count_cubes([{on, {XMin, XMax}, {YMin, YMax}, {ZMin, ZMax}} = Cube | Rest]) ->
    Count = (XMax + 1 - XMin) * (YMax + 1 - YMin) * (ZMax + 1 - ZMin),
    io:format(standard_error, "Count: ~p = ~p~n", [Cube, Count]),
    Count + count_cubes(Rest).

do_solve([{on, _, _, _} = First | Instrs]) ->
    do_solve(Instrs, [First]).

do_solve([], NonOverlapping) ->
    NonOverlapping;
do_solve([A | Rest], NonOverlapping) ->
    ?assert(not has_overlapping_cuboids(NonOverlapping)),

    io:format(standard_error, "--~nCurrent: ~p~n", [A]),
    io:format(standard_error, "Non-overlapping: ~p~n", [NonOverlapping]),

    NewNonOverlaps =
        lists:foldl(fun(B, Acc) ->
                       case split_cuboids(B, A) of
                           false -> Acc ++ [B];
                           Split -> Acc ++ Split
                       end
                    end,
                    [],
                    NonOverlapping),

    io:format(standard_error, "New non-overlapping: ~p~n", [NewNonOverlaps]),

    ?assert(not has_overlapping_cuboids(NewNonOverlaps)),

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

    io:format(standard_error, "~nA: ~p~n", [A]),
    io:format(standard_error, "B: ~p~n", [B]),

    case is_valid_cuboid(OverlapCuboid) of
        false ->
            false;
        true ->
            CX = if MinXA =< MinXB andalso MaxXA < MaxXB ->
                        % A: |-----|    ->  |--|
                        % B:    |-----|             |--|
                        % +:                    |--|
                        [{OnOffA, {MinXA, OverlapXMin - 1}, YRangeA, ZRangeA},
                         {OnOffB, {OverlapXMax + 1, MaxXB}, YRangeB, ZRangeB}];
                    MinXB =< MinXA andalso MaxXB < MaxXA ->
                        % A:    |-----|    ->         |--|
                        % B: |-----|          |--|
                        % +:                      |--|
                        [{OnOffB, {MinXB, OverlapXMin - 1}, YRangeA, ZRangeA},
                         {OnOffA, {OverlapXMax + 1, MaxXA}, YRangeB, ZRangeB}];
                    MinXA < MinXB andalso MaxXA > MaxXB ->
                        % A: |-----------| -> |--|    |--|
                        % B:    |-----|
                        % +:                      |--|
                        [{OnOffA, {MinXA, OverlapXMin - 1}, YRangeA, ZRangeA},
                         {OnOffA, {OverlapXMax + 1, MaxXA}, YRangeA, ZRangeA}];
                    MinXB < MinXA andalso MaxXB > MaxXA ->
                        % A:    |-----|    ->
                        % B: |-----------|    |--|    |--|
                        % +:                      |--|
                        [{OnOffB, {MinXB, OverlapXMin - 1}, YRangeB, ZRangeB},
                         {OnOffB, {OverlapXMax + 1, MaxXB}, YRangeB, ZRangeB}]
                 end,
            CY = if MinYA =< MinYB andalso MaxYA < MaxYB ->
                        % A overlapping left of B
                        [{OnOffA, OverlapXRange, {MinYA, OverlapYMin - 1}, ZRangeA},
                         {OnOffB, OverlapXRange, {OverlapYMax + 1, MaxYB}, ZRangeB}];
                    MinYB =< MinYA andalso MaxYB < MaxYA ->
                        % A overlapping right of B
                        [{OnOffB, OverlapXRange, {MinYB, OverlapYMin - 1}, ZRangeA},
                         {OnOffA, OverlapXRange, {OverlapYMax + 1, MaxYA}, ZRangeB}];
                    MinYA < MinYB andalso MaxYA > MaxYB ->
                        % A encloses B
                        [{OnOffA, OverlapXRange, {MinYA, OverlapYMin - 1}, YRangeA},
                         {OnOffA, OverlapXRange, {OverlapYMax + 1, MaxYA}, YRangeA}];
                    MinYB < MinYA andalso MaxYB > MaxYA ->
                        % B encloses A,
                        [{OnOffB, OverlapXRange, {MinYB, OverlapYMin - 1}, YRangeB},
                         {OnOffB, OverlapXRange, {OverlapYMax + 1, MaxYB}, YRangeB}]
                 end,
            CZ = if MinZA =< MinZB andalso MaxZA < MaxZB ->
                        % A left of B
                        [{OnOffA, OverlapXRange, OverlapYRange, {MinZA, OverlapZMin - 1}},
                         {OnOffB, OverlapXRange, OverlapYRange, {OverlapZMax + 1, MaxZB}}];
                    MinZB =< MinZA andalso MaxZB < MaxZA ->
                        % A right of B
                        [{OnOffB, OverlapXRange, OverlapYRange, {MinZB, OverlapZMin - 1}},
                         {OnOffA, OverlapXRange, OverlapYRange, {OverlapZMax + 1, MaxZA}}];
                    MinZA < MinZB andalso MaxZA > MaxZB ->
                        % A encloses B
                        [{OnOffA, OverlapXRange, OverlapYRange, {MinZA, OverlapZMin - 1}},
                         {OnOffA, OverlapXRange, OverlapYRange, {OverlapZMax + 1, MaxZA}}];
                    MinZB < MinZA andalso MaxZB > MaxZA ->
                        % B encloses A
                        [{OnOffB, OverlapXRange, OverlapYRange, {MinZB, OverlapZMin - 1}},
                         {OnOffB, OverlapXRange, OverlapYRange, {OverlapZMax + 1, MaxZB}}]
                 end,
            L = lists:filter(fun is_valid_cuboid/1, CX ++ CY ++ CZ ++ [OverlapCuboid]),
            ?assertNot(has_overlapping_cuboids(L)),
            L
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
                         io:format(standard_error, "*** Overlapping:~n*** ~p~n*** ~p~n", [A, B]),
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

% split_cuboids_test() ->
%     Split = split_cuboids({on, {0, 10}, {0, 10}, {0, 10}}, {off, {5, 15}, {5, 15}, {5, 15}}),
%     Count = count_cubes(Split),
%     ?assertNot(has_overlapping_cuboids(Split)),
%     ?assertEqual(1115, Count).

% ex1_test() ->
%     Binary =
%         <<"on x=10..12,y=10..12,z=10..12\non x=11..13,y=11..13,z=11..13\noff "
%           "x=9..11,y=9..11,z=9..11\non x=10..10,y=10..10,z=10..10">>,
%     X = solve1(parse(Binary)),
%     ?debugFmt("~n~p", [X]).

solve_test() ->
    ?assertEqual(1115,
                 solve1([{on, {0, 10}, {0, 10}, {0, 10}}, {off, {5, 15}, {5, 15}, {5, 15}}])),
    ?assertEqual(11 * 11 * 11 - 1,
                 solve1([{on, {0, 10}, {0, 10}, {0, 10}}, {off, {0, 0}, {0, 0}, {0, 0}}])).

-endif.
