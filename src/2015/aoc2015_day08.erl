-module(aoc2015_day08).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 8,
                name = "Matchsticks",
                expected = {1371, 2117},
                has_input_file = true}.

-type input_type() :: [nonempty_string()].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    string:tokens(binary_to_list(Input), "\n\r").

-spec solve1(List :: input_type()) -> result1_type().
solve1(List) ->
    lists:foldl(fun({LSize, MSize}, Acc) -> LSize - MSize + Acc end,
                0,
                [measure(Str) || Str <- List]).

-spec solve2(List :: input_type()) -> result2_type().
solve2(List) ->
    lists:foldl(fun({Orig, Quoted}, Acc) -> Acc + (length(Quoted) - length(Orig)) end,
                0,
                [{Str, "\"" ++ quote(Str) ++ "\""} || Str <- List]).

measure(Str) ->
    measure(Str, before).

measure([], 'after') ->
    {0, 0};
measure([$" | Rest], before) ->
    {LSize, MSize} = measure(Rest, inside),
    {LSize + 1, MSize};
measure([$" | Rest], inside) ->
    {LSize, MSize} = measure(Rest, 'after'),
    {LSize + 1, MSize};
measure([$\\, $x, _, _ | Rest], inside) ->
    %% Hexadecimal literal: \xDD
    {LSize, MSize} = measure(Rest, inside),
    {LSize + 4, MSize + 1};
measure([$\\, $" | Rest], inside) ->
    %% Quoted literal: \"
    {LSize, MSize} = measure(Rest, inside),
    {LSize + 2, MSize + 1};
measure([$\\, $\\ | Rest], inside) ->
    %% Quoted backslash: \\
    {LSize, MSize} = measure(Rest, inside),
    {LSize + 2, MSize + 1};
measure([_ | Rest], inside) ->
    %% Literal char
    {LSize, MSize} = measure(Rest, inside),
    {LSize + 1, MSize + 1}.

quote([]) ->
    "";
quote([$" | Rest]) ->
    [$\\, $" | quote(Rest)];
quote([$\\ | Rest]) ->
    [$\\, $\\ | quote(Rest)];
quote([C | Rest]) ->
    [C | quote(Rest)].
