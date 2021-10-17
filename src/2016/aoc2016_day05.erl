-module(aoc2016_day05).

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 5,
                name = "How About a Nice Game of Chess?",
                expected = {"4543c154", "1050cbbd"},
                use_one_solver_fun = true,
                has_input_file = false}.

-type input_type() :: string().
-type result_type() :: {string(), string()}.

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    "ojvtpuvg".

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    password(Input).

password(Input) ->
    password("", #{}, Input, 0).

password(P1, P2, Input, Index) ->
    case {length(P1), maps:size(P2)} of
        {8, 8} ->
            {lists:reverse(P1),
             lists:map(fun({_, X}) -> to_hex(X) end,
                       lists:sort(
                           maps:to_list(P2)))};
        _ ->
            case next_password_char(hash(Input, Index)) of
                {A, B} ->
                    NewP1 =
                        if length(P1) < 8 ->
                               [to_hex(A) | P1];
                           true ->
                               P1
                        end,
                    NewP2 =
                        if A < 8 ->
                               maps:update_with(A, fun(Old) -> Old end, B, P2);
                           true ->
                               P2
                        end,
                    password(NewP1, NewP2, Input, Index + 1);
                false ->
                    password(P1, P2, Input, Index + 1)
            end
    end.

to_hex(X) when X < 10 ->
    X + $0;
to_hex(X) ->
    X + $a - 10.

next_password_char(<<0, 0, 0:4, A:4, B:4, _/bitstring>>) ->
    {A, B};
next_password_char(_) ->
    false.

hash(Input, Index) ->
    erlang:md5(Input ++ integer_to_list(Index)).
