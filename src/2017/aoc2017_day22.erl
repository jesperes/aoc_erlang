-module(aoc2017_day22).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 22,
                name = "Sporifica Virus",
                expected = {5322, 2512079},
                has_input_file = true}.

-type input_type() :: {map(), {integer(), integer()}}.
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    {W, _} = binary:match(Binary, <<"\n">>),
    Map = lists:foldl(fun({N, _}, Map) ->
                         maps:put({N rem (W + 1), N div (W + 1)}, infected, Map)
                      end,
                      #{},
                      binary:matches(Binary, <<"#">>)),
    Center = W div 2,
    {Map, {Center, Center}}.

-spec solve1(Input :: input_type()) -> result_type().
solve1({Grid, Start}) ->
    iterate(Grid, Start, 0, 10000, 0).

-spec solve2(Input :: input_type()) -> result_type().
solve2({Grid, Start}) ->
    iterate2(Grid, Start, 0, 10000000, 0).

iterate(_, _, _, 0, Count) ->
    Count;
iterate(Grid, Pos, Dir, N, Count) ->
    case maps:get(Pos, Grid, clean) of
        infected ->
            NewDir = turn_right(Dir),
            iterate(maps:remove(Pos, Grid), move(Pos, NewDir), NewDir, N - 1, Count);
        clean ->
            NewDir = turn_left(Dir),
            iterate(maps:put(Pos, infected, Grid), move(Pos, NewDir), NewDir, N - 1, Count + 1)
    end.

iterate2(_, _, _, 0, Count) ->
    Count;
iterate2(Grid, Pos, Dir, N, Count) ->
    case maps:get(Pos, Grid, clean) of
        clean ->
            NewDir = turn_left(Dir),
            iterate2(maps:put(Pos, weakened, Grid), move(Pos, NewDir), NewDir, N - 1, Count);
        weakened ->
            iterate2(maps:put(Pos, infected, Grid), move(Pos, Dir), Dir, N - 1, Count + 1);
        infected ->
            NewDir = turn_right(Dir),
            iterate2(maps:put(Pos, flagged, Grid), move(Pos, NewDir), NewDir, N - 1, Count);
        flagged ->
            NewDir = reverse(Dir),
            iterate2(maps:remove(Pos, Grid), move(Pos, NewDir), NewDir, N - 1, Count)
    end.

turn_right(Dir) ->
    (Dir + 1) rem 4.

turn_left(Dir) ->
    (Dir + 3) rem 4.

reverse(Dir) ->
    (Dir + 2) rem 4.

move({X, Y}, 0) ->
    {X, Y - 1};
move({X, Y}, 1) ->
    {X + 1, Y};
move({X, Y}, 2) ->
    {X, Y + 1};
move({X, Y}, 3) ->
    {X - 1, Y}.
