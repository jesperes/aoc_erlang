-module(aoc2016_day16).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 16,
                name = "Dragon Checksum",
                expected = {"10100101010101101", "01100001101101001"},
                has_input_file = false}.

-type input_type() :: string().
-type result1_type() :: string().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    "11101000110010100".

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    fill_disk(Input, 272).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    fill_disk(Input, 35651584).

%% Replace all 0 <-> 1 and reverse the list at the same time.
rinvert([], Acc) ->
    Acc;
rinvert([$1 | Xs], Acc) ->
    rinvert(Xs, [$0 | Acc]);
rinvert([$0 | Xs], Acc) ->
    rinvert(Xs, [$1 | Acc]).

iterate(A) ->
    A ++ "0" ++ rinvert(A, []).

iterate(A, Size) ->
    A0 = iterate(A),
    if length(A0) >= Size ->
           A0;
       true ->
           iterate(A0, Size)
    end.

checksum(S) ->
    C0 = checksum0(S),
    if length(C0) rem 2 == 0 ->
           checksum(C0);
       true ->
           C0
    end.

checksum0([]) ->
    [];
checksum0([A, A | Xs]) ->
    "1" ++ checksum0(Xs);
checksum0([A, B | Xs]) when A /= B ->
    "0" ++ checksum0(Xs).

fill_disk(InitState, Size) ->
    S = iterate(InitState, Size),
    S0 = lists:sublist(S, Size),
    checksum(S0).
