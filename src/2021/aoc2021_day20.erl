-module(aoc2021_day20).

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 20,
                name = "Trench Map",
                expected = {5479, 19012},
                has_input_file = true,
                use_one_solver_fun = true}.

-type input_type() :: any().
-type result_type() :: {integer(), integer()}.

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    [Algo, Rest] = binary:split(Binary, <<"\n\n">>, [trim_all]),
    {LineLength, _} = binary:match(Rest, <<"\n">>),
    LL = LineLength + 1,
    Image =
        lists:foldl(fun({Offset, _}, CoordMap) ->
                       X = Offset rem LL,
                       Y = Offset div LL,
                       maps:put({X, Y}, 1, CoordMap)
                    end,
                    #{},
                    binary:matches(Rest, <<"#">>)),
    {Algo, Image}.

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    Input0 = {_, Map} = enhance(Input, 2),
    P1 = maps:size(Map),
    {_, Map2} = enhance(Input0, 48),
    P2 = maps:size(Map2),
    {P1, P2}.

enhance({_, Map} = Input, N) ->
    {Xs, Ys} =
        lists:unzip(
            maps:keys(Map)),

    MinX = lists:min(Xs) - N,
    MaxX = lists:max(Xs) + N,
    MinY = lists:min(Ys) - N,
    MaxY = lists:max(Ys) + N,
    Limits = {MinX, MaxX, MinY, MaxY},

    Coords = [{X, Y} || X <- lists:seq(MinX, MaxX), Y <- lists:seq(MinY, MaxY)],

    enhance(Input, N, Coords, Limits).

enhance(Input, 0, _Coords, _Limits) ->
    Input;
enhance({_, _Map0} = Input, N, Coords, Limits) ->
    {_, _Map} = Input0 = do_enhance(Input, N, Coords, Limits),
    enhance(Input0, N - 1, Coords, Limits).

do_enhance({Algo, ImageMap}, N, Coords, Limits) ->
    {Algo,
     lists:foldl(fun(Coord, Acc) ->
                    case enhance_pixel(Coord, Algo, ImageMap, N, Limits) of
                        $. -> Acc;
                        $# -> maps:put(Coord, 1, Acc)
                    end
                 end,
                 #{},
                 Coords)}.

enhance_pixel({X, Y},
              <<Algo0, _/binary>> = Algo,
              ImageMap,
              N,
              {MinX, MaxX, MinY, MaxY}) ->
    Nbrs =
        [{X - 1, Y - 1},
         {X, Y - 1},
         {X + 1, Y - 1},
         {X - 1, Y},
         {X, Y},
         {X + 1, Y},
         {X - 1, Y + 1},
         {X, Y + 1},
         {X + 1, Y + 1}],
    IsAlgo0Set = Algo0 =:= $#,
    Int = lists:foldl(fun ({X0, Y0} = Coord, Acc)
                              when X0 >= MinX
                                   andalso X0 =< MaxX
                                   andalso Y0 >= MinY
                                   andalso Y0 =< MaxY ->
                              Acc bsl 1 bor maps:get(Coord, ImageMap, 0);
                          (_, Acc) when N rem 2 == 1 andalso IsAlgo0Set ->
                              Acc bsl 1 bor 1;
                          (_, Acc) ->
                              Acc bsl 1
                      end,
                      0,
                      Nbrs),
    binary:at(Algo, Int).

%% Tests
-ifdef(TEST).

ex1_test() ->
    Binary =
        <<"..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#.."
          "##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#."
          "#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###.."
          "#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#"
          "..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#"
          ".......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####...."
          "##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#."
          ".###..#####........#..####......#..#\n\n#..#.\n#....\n##..#\n..#..\n"
          "..###">>,

    %% safeguard against accidental editing
    ?assertEqual(543, size(Binary)),

    Input = parse(Binary),
    {_, X1} = enhance(Input, 2),
    ?assertEqual(35, maps:size(X1)),

    {_, X2} = enhance(Input, 50),
    ?assertEqual(3351, maps:size(X2)).

-endif.
