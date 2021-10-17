-module(aoc2019_day16).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 16,
                name = "Flawed Frequency Transmission",
                expected = {"84970726", "47664469"},
                has_input_file = true}.

-type input_type() :: string().
-type result_type() :: string().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    string:trim(binary_to_list(Binary)).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    fft(Input, 100).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    fft2(Input, 100).

fft(String, N) ->
    Digits = to_digits(String),
    Fft = lists:foldl(fun(_, Acc) -> fft(Acc) end, Digits, lists:seq(1, N)),
    {F, _} = lists:split(8, to_string(Fft)),
    F.

fft(Digits) ->
    Len = length(Digits),
    io:format("~n---~n", []),
    lists:map(fun(SegLen) ->
                 List = lists:zip(Digits, lists:seq(0, length(Digits) - 1)),
                 abs(lists:foldl(fun({Digit, Pos}, Acc) -> Digit * pattern(Pos, SegLen) + Acc end,
                                 0,
                                 List))
                 rem 10
              end,
              lists:seq(1, Len)).

pattern(Pos, SegLen) ->
    case (Pos + 1) div SegLen rem 4 of
        0 ->
            0;
        1 ->
            1;
        2 ->
            0;
        3 ->
            -1
    end.

%% Return a list of N, repeated M times.
repeat(N, M) ->
    lists:flatten([N || _ <- lists:seq(1, M)]).

to_digits(S) ->
    lists:map(fun(C) -> C - $0 end, S).

to_string(L) ->
    lists:map(fun(C) -> $0 + C end, L).

%% For part 2, we only care about digits at an offset which is in the
%% second half of the string, which allows us to optimize things.
fft2(String, N) ->
    {S, _} = lists:split(7, String),
    Offset = list_to_integer(S),
    {_, Digits} = lists:split(Offset, repeat(to_digits(String), 10000)),
    FFT = lists:foldl(fun(_, Acc) -> rfft2(Acc) end, Digits, lists:seq(1, N)),
    {F, _} = lists:split(8, FFT),
    to_string(F).

%% This is basically a mapfoldr, but slightly faster.
rfft2(Digits) ->
    {_, Digits0} = do_rfft2(Digits),
    Digits0.

do_rfft2([]) ->
    {0, []};
do_rfft2([D | Digits]) ->
    {D0, Digits0} = do_rfft2(Digits),
    D1 = (D0 + D) rem 10,
    {D1, [D1 | Digits0]}.
