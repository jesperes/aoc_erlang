-module(aoc2016_day21).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 21,
                name = "Scrambled Letters and Hash",
                expected = {"cbeghdaf", "bacdefgh"},
                has_input_file = true}.

-type input_type() :: [string()].
-type result1_type() :: string().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    string:tokens(binary_to_list(Input), "\n\r").

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    scramble("abcdefgh", Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    [U] = lists:filter(fun(P) -> scramble(P, Input) =:= "fbgdceah" end, permute("abcdefgh")),
    U.

-spec permute([any()]) -> [[any()]].
permute([]) ->
    [[]];
permute(L) ->
    [[X | Y] || X <- L, Y <- permute(L -- [X])].

%% Scrambler
scramble(Password, []) ->
    Password;
scramble(Password, [Instr | Instrs]) ->
    I = fun list_to_integer/1,
    P0 = case string:tokens(Instr, " ") of
             ["rotate", "based", _, _, _, _, [Letter]] ->
                 rotate_pos(Password, Letter);
             ["rotate", "left", Steps, _] ->
                 rotate_left(Password, I(Steps));
             ["rotate", "right", Steps, _] ->
                 rotate_right(Password, I(Steps));
             ["swap", "position", PosA, _, _, PosB] ->
                 swap_pos(Password, I(PosA), I(PosB));
             ["swap", "letter", [LetterA], _, _, [LetterB]] ->
                 swap(Password, LetterA, LetterB);
             ["move", "position", PosFrom, _, _, PosTo] ->
                 move(Password, I(PosFrom), I(PosTo));
             ["reverse", _, PosFrom, _, PosTo] ->
                 reverse(Password, I(PosFrom), I(PosTo))
         end,
    scramble(P0, Instrs).

%% String ops
swap([], _, _) ->
    [];
swap([A | Rest], A, B) ->
    [B | swap(Rest, A, B)];
swap([B | Rest], A, B) ->
    [A | swap(Rest, A, B)];
swap([C | Rest], A, B) ->
    [C | swap(Rest, A, B)].

swap_pos(S, A, B) ->
    X = lists:nth(A + 1, S),
    Y = lists:nth(B + 1, S),
    swap(S, X, Y).

reverse(S, A, B) ->
    Before = lists:sublist(S, 1, A),
    Reverse = lists:sublist(S, A + 1, B - A + 1),
    After = lists:sublist(S, B + 2, length(S)),
    Before ++ lists:reverse(Reverse) ++ After.

rotate_left(S, A) when A >= length(S) ->
    rotate_left(S, A rem length(S));
rotate_left(S, A) ->
    lists:sublist(S, A + 1, length(S)) ++ lists:sublist(S, 1, A).

rotate_right(S, A) when A >= length(S) ->
    rotate_right(S, A rem length(S));
rotate_right(S, A) ->
    lists:sublist(S, length(S) - A + 1, length(S)) ++ lists:sublist(S, 1, length(S) - A).

indexof(S, A) ->
    indexof(S, A, 0).

indexof([], _, _) ->
    false;
indexof([A | _], A, N) ->
    N;
indexof([_ | S], A, N) ->
    indexof(S, A, N + 1).

rotate_pos(S, A) ->
    N = indexof(S, A),
    rotate_right(S,
                 if N >= 4 ->
                        N + 2;
                    true ->
                        N + 1
                 end).

move(S, A, B) ->
    {S1, [X | S2]} = lists:split(A, S),
    S3 = S1 ++ S2,
    {S4, S5} = lists:split(B, S3),
    S4 ++ [X | S5].
