%%% Advent of Code solution for 2019 day 08.
%%% Created: 2019-12-08T06:56:28+00:00

-module(aoc2019_day08).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-define(W, 25).
-define(H, 6).
-define(LSIZE, 150). %% Number of pixels per layer
-define(BLACK, $0).
-define(WHITE, $1).
-define(TRANSPARENT, $2).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 8,
                name = "Space Image Format",
                expected =
                    {1703,
                     %% kludge to trick code formatter
                     lists:flatten(["#  #  ##   ##  #### #### \n",
                                    "#  # #  # #  # #    #    \n",
                                    "#### #    #    ###  ###  \n",
                                    "#  # #    # ## #    #    \n",
                                    "#  # #  # #  # #    #    \n",
                                    "#  #  ##   ### #    #### \n"])},
                has_input_file = true}.

-type input_type() :: any().
-type result1_type() :: integer().
-type result2_type() :: string().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    Binary.

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Binary) ->
    {_, Ones, Twos} =
        lists:min(
            lists:map(fun(Layer) ->
                         %% Count 0/1/2 in this layer
                         Fun = fun(N, {N0, N1, N2} = Acc) ->
                                  case binary:at(Binary, N) of
                                      $0 -> {N0 + 1, N1, N2};
                                      $1 -> {N0, N1 + 1, N2};
                                      $2 -> {N0, N1, N2 + 1};
                                      _ -> Acc
                                  end
                               end,
                         lists:foldl(Fun,
                                     {0, 0, 0},
                                     lists:seq(Layer * ?LSIZE, (Layer + 1) * ?LSIZE - 1))
                      end,
                      layers())),
    Ones * Twos.

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Binary) ->
    lists:flatten([[pixel_at({X, Y}, Binary) || X <- lists:seq(0, ?W - 1)] ++ "\n"
                   || Y <- lists:seq(0, ?H - 1)]).

layers() ->
    lists:seq(0, 99).

pixel_at({X, Y}, Binary) ->
    compose(lists:map(fun(Layer) -> binary:at(Binary, Layer * ?LSIZE + Y * ?W + X) end,
                      layers())).

compose([?WHITE | _]) ->
    $#;
compose([?BLACK | _]) ->
    $\ ;
compose([?TRANSPARENT | Rest]) ->
    compose(Rest).
