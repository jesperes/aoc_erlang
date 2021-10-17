-module(aoc2016_day04).

-include("aoc_puzzle.hrl").

-define(STORAGE_NAME, "northpole-object-storage").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 4,
                name = "Security Through Obscurity",
                expected = {185371, 984},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: [string()].
-type result_type() :: {integer(), integer()}.

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    string:tokens(binary_to_list(Input), "\n\r").

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    lists:foldl(fun(Str, {Acc0, Acc1}) ->
                   Room = room(Str),
                   {case Room of
                        {Id1, X, X, _} -> Acc0 + Id1;
                        _ -> Acc0
                    end,
                    case Room of
                        {Id2, _, _, ?STORAGE_NAME} -> Id2;
                        _ -> Acc1
                    end}
                end,
                {0, undef},
                Input).

encrypted_name(Str) ->
    EncrName = lists:takewhile(fun(C) -> (C =:= $-) or (C >= $a) and (C =< $z) end, Str),
    lists:sublist(EncrName, 1, length(EncrName) - 1).

room(Str) ->
    Tokens = string:tokens(Str, "-[]"),
    [ChkSum, ID | _] = lists:reverse(Tokens),
    IdInt = list_to_integer(ID),
    EncrName = encrypted_name(Str),
    {IdInt, lists:flatten(ChkSum), most_common(EncrName), decrypt(EncrName, IdInt)}.

most_common(Str) ->
    FreqTable = freq_table(Str),
    lists:map(fun({_, K}) -> K end,
              lists:sublist(
                  lists:sort(
                      lists:map(fun({K, V}) -> {-V, K} end, maps:to_list(FreqTable))),
                  5)).

decrypt(Str, Key) ->
    decrypt(Str, Key, []).

decrypt([], _, Acc) ->
    lists:reverse(Acc);
decrypt([$- | Rest], Key, Acc) ->
    decrypt(Rest, Key, [$- | Acc]);
decrypt([C | Rest], Key, Acc) ->
    D = (C - $a + Key) rem 26 + $a,
    decrypt(Rest, Key, [D | Acc]).

freq_table(Str) ->
    freq_table(Str, #{}).

freq_table([], Map) ->
    Map;
freq_table([$- | Rest], Map) ->
    freq_table(Rest, Map);
freq_table([C | Rest], Map) ->
    freq_table(Rest, maps:update_with(C, fun(V) -> V + 1 end, 1, Map)).
