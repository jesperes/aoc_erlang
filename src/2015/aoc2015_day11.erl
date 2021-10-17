-module(aoc2015_day11).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 11,
                name = "Corporate Policy",
                expected = {"cqjxxyzz", "cqkaabcc"},
                has_input_file = false}.

-type input_type() :: string().
-type result1_type() :: string().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    "cqjxjnds".

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    next_valid_password(Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(_) ->
    {P1, _} = (info())#aoc_puzzle.expected,
    next_valid_password(P1).

rev(S) ->
    lists:reverse(S).

increment(Pwd) ->
    rev(increment0(rev(Pwd))).

increment0([]) ->
    [];
increment0([$z | Xs]) ->
    [$a | increment0(Xs)];
increment0([C | Xs]) ->
    [C + 1 | Xs].

has_straight([]) ->
    false;
has_straight([X, Y, Z | _]) when (Y == X + 1) and (Z == Y + 1) ->
    true;
has_straight([_ | Rest]) ->
    has_straight(Rest).

no_confusing_letters(Pwd) ->
    not (lists:member($i, Pwd) or lists:member($o, Pwd) or lists:member($l, Pwd)).

%% Password are always 8 chars, so there are only a handful of
%% combinations of having two distinct pairs.
two_pairs([A, A, B, B, _, _, _, _]) when A =/= B ->
    true;
two_pairs([A, A, _, B, B, _, _, _]) when A =/= B ->
    true;
two_pairs([A, A, _, _, B, B, _, _]) when A =/= B ->
    true;
two_pairs([A, A, _, _, _, B, B, _]) when A =/= B ->
    true;
two_pairs([A, A, _, _, _, _, B, B]) when A =/= B ->
    true;
two_pairs([_, A, A, B, B, _, _, _]) when A =/= B ->
    true;
two_pairs([_, A, A, _, B, B, _, _]) when A =/= B ->
    true;
two_pairs([_, A, A, _, _, B, B, _]) when A =/= B ->
    true;
two_pairs([_, A, A, _, _, _, B, B]) when A =/= B ->
    true;
two_pairs([_, _, A, A, B, B, _, _]) when A =/= B ->
    true;
two_pairs([_, _, A, A, _, B, B, _]) when A =/= B ->
    true;
two_pairs([_, _, A, A, _, _, B, B]) when A =/= B ->
    true;
two_pairs([_, _, _, A, A, B, B, _]) when A =/= B ->
    true;
two_pairs([_, _, _, A, A, _, B, B]) when A =/= B ->
    true;
two_pairs([_, _, _, _, A, A, B, B]) when A =/= B ->
    true;
two_pairs(_) ->
    false.

valid(Pwd) ->
    %% * Passwords must include one increasing straight of at least
    %%   three letters, like abc, bcd, cde, and so on, up to xyz. They
    %%   cannot skip letters; abd doesn't count.
    %%
    %% * Passwords may not contain the letters i, o, or l, as these
    %%   letters can be mistaken for other characters and are
    %%   therefore confusing.
    %%
    %% * Passwords must contain at least two different,
    %%   non-overlapping pairs of letters, like aa, bb, or zz.
    has_straight(Pwd) andalso no_confusing_letters(Pwd) andalso two_pairs(Pwd).

next_valid_password(Pwd) ->
    Next = increment(Pwd),
    case valid(Next) of
        true ->
            Next;
        false ->
            next_valid_password(increment(Next))
    end.
