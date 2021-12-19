-module(aoc2021_day19).

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-compile([nowarn_unused_function]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 19,
                name = "Beacon Scanner",
                expected = {454, 10813},
                has_input_file = true,
                use_one_solver_fun = true}.

-type coord() :: {integer(), integer(), integer()}.
-type distance_vector() :: {integer(), integer(), integer()}.
-type scanner() :: {Id :: integer(), Coords :: [coord()]}.
-type input_type() :: [scanner()].
-type result_type() :: {integer(), integer()}.

scanner_id({Id, _}) ->
    Id.

scanner_coords({_, Coords}) ->
    Coords.

scanner(Id, Coords) ->
    {Id, Coords}.

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    F = fun binary_to_integer/1,
    Split = fun(B, C) -> binary:split(B, C, [trim_all, global]) end,
    lists:map(fun(B) ->
                 [Header | Coords] = Split(B, <<"\n">>),
                 [_, _, ScannerNum, _] = Split(Header, <<" ">>),
                 {F(ScannerNum),
                  lists:map(fun(CoordBin) ->
                               [X, Y, Z] = Split(CoordBin, <<",">>),
                               {F(X), F(Y), F(Z)}
                            end,
                            Coords)}
              end,
              Split(Binary, <<"\n\n">>)).

-spec solve(Input :: input_type()) -> result_type().
solve(Scanners) ->
    {MergedScanner, ScannerPositions} = merge(Scanners, [], rotation_funs()),
    MaxManhattanDist =
        lists:max([manhattan_dist(DV1, DV2) || DV1 <- ScannerPositions, DV2 <- ScannerPositions]),
    {length(scanner_coords(MergedScanner)), MaxManhattanDist}.

-spec merge([scanner()], list(), [fun()]) -> {[scanner()], list()}.
merge([A], ScannerPositions, _RotationFuns) ->
    {A, ScannerPositions};
merge([A, B | Rest], ScannerPositions, RotationFuns) ->
    case find_overlap(A, B, RotationFuns) of
        false ->
            merge([A] ++ Rest ++ [B], ScannerPositions, RotationFuns);
        {{Dx, Dy, Dz} = DV, RotatedB} ->
            RemappedCoords =
                lists:map(fun({X1, Y1, Z1}) -> {X1 + Dx, Y1 + Dy, Z1 + Dz} end,
                          scanner_coords(RotatedB)),
            MergedCoords = lists:usort(RemappedCoords ++ scanner_coords(A)),
            merge([scanner(scanner_id(A), MergedCoords) | Rest],
                  [DV | ScannerPositions],
                  RotationFuns)
    end.

-spec distance_vector(coord(), coord()) -> distance_vector().
distance_vector({X0, Y0, Z0}, {X1, Y1, Z1}) ->
    {X0 - X1, Y0 - Y1, Z0 - Z1}.

has_12_identical_elems([]) ->
    false;
has_12_identical_elems([X, X, X, X, X, X, X, X, X, X, X, X | _]) ->
    {true, X};
has_12_identical_elems([_ | Rest]) ->
    has_12_identical_elems(Rest).

% Check if the set of scanner coordinates A and B overlap by at least 12 points.
% If so, return {DistanceVector, RotatedCoordinates}. Uses the process
% dictionary to cache rotations.
-spec find_overlap(A :: scanner(), B :: scanner(), RotationFuns :: [fun()]) ->
                      {DV :: distance_vector(), RotatedB :: scanner()} | false.
find_overlap(_, _, []) ->
    false;
find_overlap(A, B, [RotationFun | Fs]) ->
    Key = {scanner_id(B), RotationFun},
    case get(Key) of
        undefined ->
            case do_find_overlap(A, B, RotationFun) of
                false ->
                    find_overlap(A, B, Fs);
                Result ->
                    put(Key, Result),
                    Result
            end;
        Result ->
            Result
    end.

-spec do_find_overlap(A :: scanner(), B :: scanner(), RotationFun :: fun()) ->
                         false | {DV :: distance_vector(), RotatedB :: scanner()}.
do_find_overlap(A, B, RotationFun) ->
    RotatedBCoords = lists:map(fun(Coord) -> RotationFun(Coord) end, scanner_coords(B)),
    DVs = [distance_vector(C1, C2) || C1 <- scanner_coords(A), C2 <- RotatedBCoords],
    case has_12_identical_elems(lists:sort(DVs)) of
        {true, DV} ->
            {DV, scanner(scanner_id(B), RotatedBCoords)};
        false ->
            false
    end.

count_rotation(Coords, Fun) ->
    Key = {overlap_count, Coords, Fun},
    case get(Key) of
        undefined ->
            put(Key, 1);
        Value ->
            put(Key, Value + 1)
    end.

manhattan_dist({X0, Y0, Z0}, {X1, Y1, Z1}) ->
    abs(X0 - X1) + abs(Y0 - Y1) + abs(Z0 - Z1).

%% 24 possible rotations
%% Checked against https://github.com/mytbk/advent_of_code/blob/main/2021/19/positions-transforms.ads
rotation_funs() ->
    [%% 1
     fun({X, Y, Z}) -> {X, Y, Z} end,
     fun({X, Y, Z}) -> {X, -Y, -Z} end,
     fun({X, Y, Z}) -> {X, Z, -Y} end,
     fun({X, Y, Z}) -> {X, -Z, Y} end,
     fun({X, Y, Z}) -> {-X, Y, -Z} end,
     %% 6
     fun({X, Y, Z}) -> {-X, -Y, Z} end,
     fun({X, Y, Z}) -> {-X, Z, Y} end,
     fun({X, Y, Z}) -> {-X, -Z, -Y} end,
     fun({X, Y, Z}) -> {Y, X, -Z} end,
     fun({X, Y, Z}) -> {Y, -X, Z} end,
     %% 11
     fun({X, Y, Z}) -> {Y, Z, X} end,
     fun({X, Y, Z}) -> {Y, -Z, -X} end,
     fun({X, Y, Z}) -> {-Y, X, Z} end,
     fun({X, Y, Z}) -> {-Y, -X, -Z} end,
     fun({X, Y, Z}) -> {-Y, Z, -X} end,
     %% 16
     fun({X, Y, Z}) -> {-Y, -Z, X} end,
     fun({X, Y, Z}) -> {Z, X, Y} end,
     fun({X, Y, Z}) -> {Z, -X, -Y} end,
     fun({X, Y, Z}) -> {Z, Y, -X} end,
     fun({X, Y, Z}) -> {Z, -Y, X} end,
     %% 21
     fun({X, Y, Z}) -> {-Z, X, -Y} end,
     fun({X, Y, Z}) -> {-Z, -X, Y} end,
     fun({X, Y, Z}) -> {-Z, Y, X} end,
     fun({X, Y, Z}) -> {-Z, -Y, -X} end].

%% Tests

-ifdef(TEST).

ex1_test() ->
    %% Oh lord, erlfmt really fucks this up. Anyway, this is the big example for part 1.
    Scanners =
        parse(<<"--- scanner 0 ---\n404,-588,-901\n528,-643,409\n-838,591,734\n390,-6"
                "75,-793\n-537,-823,-458\n-485,-357,347\n-345,-311,381\n-661,-816,-57"
                "5\n-876,649,763\n-618,-824,-621\n553,345,-567\n474,580,667\n-447,-32"
                "9,318\n-584,868,-557\n544,-627,-890\n564,392,-477\n455,729,728\n-892"
                ",524,684\n-689,845,-530\n423,-701,434\n7,-33,-71\n630,319,-379\n443,"
                "580,662\n-789,900,-551\n459,-707,401\n\n--- scanner 1 ---\n686,422,5"
                "78\n605,423,415\n515,917,-361\n-336,658,858\n95,138,22\n-476,619,847\n"
                "-340,-569,-846\n567,-361,727\n-460,603,-452\n669,-402,600\n729,430,5"
                "32\n-500,-761,534\n-322,571,750\n-466,-666,-811\n-429,-592,574\n-355"
                ",545,-477\n703,-491,-529\n-328,-685,520\n413,935,-424\n-391,539,-444\n"
                "586,-435,557\n-364,-763,-893\n807,-499,-711\n755,-354,-619\n553,889,"
                "-390\n\n--- scanner 2 ---\n649,640,665\n682,-795,504\n-784,533,-524\n"
                "-644,584,-595\n-588,-843,648\n-30,6,44\n-674,560,763\n500,723,-460\n"
                "609,671,-379\n-555,-800,653\n-675,-892,-343\n697,-426,-610\n578,704,"
                "681\n493,664,-388\n-671,-858,530\n-667,343,800\n571,-461,-707\n-138,"
                "-166,112\n-889,563,-600\n646,-828,498\n640,759,510\n-630,509,768\n-6"
                "81,-892,-333\n673,-379,-804\n-742,-814,-386\n577,-820,562\n\n--- "
                "scanner 3 ---\n-589,542,597\n605,-692,669\n-500,565,-823\n-660,373,5"
                "57\n-458,-679,-417\n-488,449,543\n-626,468,-788\n338,-750,-386\n528,"
                "-832,-391\n562,-778,733\n-938,-730,414\n543,643,-506\n-524,371,-870\n"
                "407,773,750\n-104,29,83\n378,-903,-323\n-778,-728,485\n426,699,580\n"
                "-438,-605,-362\n-469,-447,-387\n509,732,623\n647,635,-688\n-868,-804"
                ",481\n614,-800,639\n595,780,-596\n\n--- scanner 4 ---\n727,592,562\n"
                "-293,-554,779\n441,611,-461\n-714,465,-776\n-743,427,-804\n-660,-479"
                ",-426\n832,-632,460\n927,-485,-438\n408,393,-506\n466,436,-512\n110,"
                "16,151\n-258,-428,682\n-393,719,612\n-211,-452,876\n808,-476,-593\n-"
                "575,615,604\n-485,667,467\n-680,325,-822\n-627,-443,-432\n872,-547,-"
                "609\n833,512,582\n807,604,487\n839,-516,451\n891,-625,532\n-652,-548"
                ",-490\n30,-46,-14">>),

    ?assertEqual({79, 3621}, solve(Scanners)).

-endif.
