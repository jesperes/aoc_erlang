-module(aoc2016_day14).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 14,
                name = "One-Time Pad",
                expected = {23890, 22696},
                has_input_file = false}.

-type input_type() :: string().
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    "ahsbgdzn".

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    find_key(Input, fun erlang:md5/1).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    find_key(Input, fun(S) -> md5_stretched(S, 2016) end).

find_key(Salt, HashFun) ->
    find_key(Salt, HashFun, 0, #{}, []).

find_key(Salt, HashFun, N, Threes, Keys) ->
    Digest = HashFun(Salt ++ integer_to_list(N)),

    Keys0 =
        case has5(Digest) of
            false ->
                Keys;
            C5 ->
                %% Get list of indices which have a matching 3-sequence
                NewKeys = lists:filter(fun(I) -> N - I =< 1000 end, maps:get(C5, Threes, [])),
                lists:sort(NewKeys ++ Keys)
        end,

    case length(Keys0) of
        L when L >= 64 ->
            %% We have found (at least) 64 keys, so return the 64th.
            %% Eventually we must continue until we find a 5-sequence which
            %% is more than 1000 indexes larger than the largest key we
            %% have.
            lists:nth(64, Keys0);
        _ ->
            Threes0 =
                case has3(Digest) of
                    false ->
                        Threes;
                    C3 ->
                        maps:update_with(C3, fun(Old) -> [N | Old] end, [N], Threes)
                end,

            find_key(Salt, HashFun, N + 1, Threes0, Keys0)
    end.

has3(Binary) when bit_size(Binary) < 12 ->
    false; %% Less than 3 chars left (3 * 4)
has3(<<C:4, C:4, C:4, _/bitstring>>) ->
    C;
has3(<<_:4, Rest/bitstring>>) ->
    has3(Rest).

has5(Binary) when bit_size(Binary) < 20 ->
    false; %% Less than 5 chars left (5 * 4)
has5(<<C:4, C:4, C:4, C:4, C:4, _/bitstring>>) ->
    C;
has5(<<_:4, Rest/bitstring>>) ->
    has5(Rest).

md5_stretched(S, 0) ->
    erlang:md5(S);
md5_stretched(S, N) ->
    md5_stretched(digest_to_hexstring(erlang:md5(S)), N - 1).

%% This is where this puzzle spends 90% of its time, converting
%% binaries to hexstrings. Implemented as a NIF.
digest_to_hexstring(Binary) when byte_size(Binary) == 16 ->
    aoc_nifs:digest_to_hexstring(Binary).
