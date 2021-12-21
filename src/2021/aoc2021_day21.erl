-module(aoc2021_day21).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 21,
                name = "Dirac Dice",
                expected = {432450, 138508043837521},
                has_input_file = false}.

-record(die, {next = 1 :: integer(), count = 0 :: integer()}).
-record(player, {id :: integer(), pos :: integer(), score = 0 :: integer()}).

-type input_type() :: any().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(_Binary) ->
    {#player{id = 1, pos = 1}, #player{id = 2, pos = 5}}.

-spec solve1(Input :: input_type()) -> result_type().
solve1({Player1, Player2}) ->
    play(Player1, Player2, #die{next = 1}).

-spec solve2(Input :: input_type()) -> result_type().
solve2({Player1, Player2}) ->
    {Wins, _} = play3(Player1, Player2, #{}),
    lists:max(
        maps:values(Wins)).

roll3sum(#die{next = Next, count = Count}) ->
    {Next * 3 + 3, #die{next = Next + 3, count = Count + 3}}.

play(#player{pos = Pos, score = Score} = Player, Other, Die) ->
    {Sum, Die0} = roll3sum(Die),
    Space = (Pos - 1 + Sum) rem 10 + 1,
    Player0 = Player#player{pos = Space, score = Score + Space},
    case Player0#player.score of
        N when N >= 1000 ->
            LosingScore = Other#player.score,
            RollCount = Die0#die.count,
            LosingScore * RollCount;
        _ ->
            play(Other, Player0, Die0)
    end.

play3(P1, P2, Cache) ->
    Key = {P1, P2},
    case maps:get(Key, Cache, undefined) of
        undefined ->
            {Result, CacheOut} = do_play3(P1, P2, Cache),
            {Result, maps:put(Key, Result, CacheOut)};
        Result ->
            {Result, Cache}
    end.

%% all 27 different sums of rolling the quantum die three times
rollsums() ->
    [3, 4, 5, 4, 5, 6, 5, 6, 7, 4, 5, 6, 5, 6, 7, 6, 7, 8, 5, 6, 7, 6, 7, 8, 7, 8, 9].

do_play3(#player{id = Id,
                 pos = Pos,
                 score = Score} =
             Player,
         Other,
         Cache) ->
    {WinsOut, NewCache} =
        lists:foldl(fun(Roll, {WinsIn, CacheIn}) ->
                       Space = (Pos - 1 + Roll) rem 10 + 1,
                       Player0 = Player#player{pos = Space, score = Score + Space},
                       case Player0#player.score of
                           N when N >= 21 ->
                               {maps:update_with(Id, fun(Old) -> Old + 1 end, 1, WinsIn), CacheIn};
                           _ ->
                               {Wins, CacheOut} = play3(Other, Player0, CacheIn),
                               WinsOut =
                                   #{1 => maps:get(1, Wins) + maps:get(1, WinsIn),
                                     2 => maps:get(2, Wins) + maps:get(2, WinsIn)},
                               {WinsOut, CacheOut}
                       end
                    end,
                    {#{1 => 0, 2 => 0}, Cache},
                    rollsums()),
    {WinsOut, NewCache}.

%% Tests
-ifdef(TEST).

ex1_test() ->
    ?assertEqual(739785, solve1({#player{id = 1, pos = 4}, #player{id = 2, pos = 8}})).

ex2_test() ->
    ?assertEqual(444356092776315,
                 solve2({#player{id = 1, pos = 4}, #player{id = 2, pos = 8}})).

-endif.
