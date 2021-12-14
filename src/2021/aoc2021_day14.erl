-module(aoc2021_day14).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 14,
                name = "Extended Polymerization",
                expected = {3831, 5725739914282},
                has_input_file = true}.

-type input_type() :: any().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    [Root, Rest] = binary:split(Binary, <<"\n">>),
    Rules =
        lists:foldl(fun(Bin, Acc) ->
                       [A, B] = string:tokens(binary_to_list(Bin), " ->"),
                       maps:put(A, B, Acc)
                    end,
                    #{},
                    binary:split(Rest, <<"\n">>, [trim_all, global])),
    {binary_to_list(Root), Rules}.

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    do_n_steps(Input, 10).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    do_n_steps(Input, 40).

do_n_steps({Root, Rules}, N) ->
    LetterCounts = utils:freq_map(Root),
    PairCounts = pair_freqs(Root, #{}),
    {_, FinalLetterCounts} =
        lists:foldl(fun(_, Acc) -> do_one_step2(Rules, Acc) end,
                    {PairCounts, LetterCounts},
                    lists:seq(1, N)),
    Values = maps:values(FinalLetterCounts),
    lists:max(Values) - lists:min(Values).

do_one_step2(Rules, {PairCounts, LetterCounts}) ->
    lists:foldl(fun([A, B] = Pair, {PairCountIn, LetterCountIn}) ->
                   %% When applying AB -> C, all AB:s will be replaced by an equivalent number
                   %% of AC:s and CB:s.
                   [C] = maps:get(Pair, Rules),
                   PairCount = maps:get(Pair, PairCounts),
                   PC0 = maps:update_with(Pair, fun(N) -> N - PairCount end, PairCountIn),
                   PC1 = maps:update_with([A, C], fun(N) -> N + PairCount end, PairCount, PC0),
                   PC2 = maps:update_with([C, B], fun(N) -> N + PairCount end, PairCount, PC1),
                   LC0 = maps:update_with(C, fun(N) -> N + PairCount end, PairCount, LetterCountIn),
                   {PC2, LC0}
                end,
                {PairCounts, LetterCounts},
                maps:keys(PairCounts)).

pair_freqs([_], Map) ->
    Map;
pair_freqs([A, B | Rest], Map) ->
    Key = [A, B],
    pair_freqs([B | Rest], maps:update_with(Key, fun(N) -> N + 1 end, 1, Map)).

-ifdef(TEST).

ex1_test() ->
    Input =
        parse(<<"NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> "
                "B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB "
                "-> N\nBC -> B\nCC -> N\nCN -> C">>),
    ?assertEqual(1588, do_n_steps(Input, 10)).

-endif.
