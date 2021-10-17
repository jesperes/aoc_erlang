-module(aoc2016_day07).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 7,
                name = "Internet Protocol Version 7",
                expected = {115, 231},
                has_input_file = true}.

-type input_type() :: [string()].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    string:tokens(binary_to_list(Input), "\n\r").

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    length(lists:filter(fun is_tls/1, Input)).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    length(lists:filter(fun supports_ssl/1, Input)).

%%% Returns true iff at least one "abba" sequence has been found,
%%% and no "abba" sequence has been found inside brackets.
is_tls([], _In, Found, FoundBr) ->
    Found and not FoundBr;
is_tls([X, Y, Y, X | Xs], In, _Found, FoundBr) when X /= Y ->
    is_tls(Xs, In, true, In or FoundBr);
is_tls([$[ | Xs], _In, Found, FoundBr) ->
    is_tls(Xs, true, Found, FoundBr);
is_tls([$] | Xs], _In, Found, FoundBr) ->
    is_tls(Xs, false, Found, FoundBr);
is_tls([_ | Xs], In, Found, FoundBr) ->
    is_tls(Xs, In, Found, FoundBr).

is_tls(X) ->
    is_tls(X, false, false, false).

split_ip([], _) ->
    {[], []};
split_ip([$[ | Xs], N) ->
    split_ip(Xs, N + 1);
split_ip([$] | Xs], N) ->
    split_ip(Xs, N - 1);
split_ip([X | Xs], N) when N == 0 ->
    {A, B} = split_ip(Xs, N),
    {[X | A], B};
split_ip([X | Xs], N) when N >= 0 ->
    {A, B} = split_ip(Xs, N),
    {A, [X | B]}.

split_ip(Ip) ->
    split_ip(Ip, 0).

find_aba([], Acc) ->
    Acc;
find_aba([X, Y, X | Xs], Acc) when X /= Y ->
    find_aba([Y, X | Xs], [[X, Y, X] | Acc]);
find_aba([_ | Xs], Acc) ->
    find_aba(Xs, Acc).

find_bab([], _) ->
    false;
find_bab([X | Abas], Str) ->
    [A, B, A] = X,
    case string:find(Str, [B, A, B]) of
        nomatch ->
            find_bab(Abas, Str);
        _ ->
            true
    end.

supports_ssl(X) ->
    {A, B} = split_ip(X),
    Abas = find_aba(A, []),
    find_bab(Abas, B).
