-module(aoc2019_day22).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 22,
                name = "Slam Shuffle",
                expected = {4775, 37889219674304},
                has_input_file = true}.

-type instr() :: {cut, integer()} | {incr, integer()} | new.
-type input_type() :: [instr()].
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    Lines = string:tokens(binary_to_list(Binary), "\n\r"),
    ToI = fun list_to_integer/1,
    lists:map(fun(Line) ->
                 case string:tokens(Line, " ") of
                     ["cut", Cut] -> {cut, ToI(Cut)};
                     ["deal", "with", "increment", Incr] -> {incr, ToI(Incr)};
                     ["deal", "into", "new", "stack"] -> new
                 end
              end,
              Lines).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    part1(Input).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    part2(Input).

part1(Instrs) ->
    Deck = shuffle(Instrs, lists:seq(0, 10006)),
    find_card(2019, Deck, 0).

find_card(_, [], _) ->
    false;
find_card(Card, [Card | _], N) ->
    N;
find_card(Card, [_ | Deck], N) ->
    find_card(Card, Deck, N + 1).

shuffle([], Deck) ->
    Deck;
shuffle([Instr | Instrs], Deck) ->
    case Instr of
        {cut, Cut} when Cut >= 0 ->
            {L1, L2} = lists:split(Cut, Deck),
            shuffle(Instrs, L2 ++ L1);
        {cut, Cut} when Cut < 0 ->
            {L1, L2} = lists:split(length(Deck) + Cut, Deck),
            shuffle(Instrs, L2 ++ L1);
        {incr, Incr} ->
            Len = length(Deck),
            {Positions, _} = lists:split(Len, lists:seq(0, Len * Incr, Incr)),
            Tree =
                lists:foldl(fun({Pos, Card}, Acc) ->
                               Index = Pos rem Len,
                               gb_trees:insert(Index, Card, Acc)
                            end,
                            gb_trees:empty(),
                            lists:zip(Positions, Deck)),
            Deck0 = gb_trees:values(Tree),
            shuffle(Instrs, Deck0);
        new ->
            shuffle(Instrs, lists:reverse(Deck))
    end.

%% For part 2 we need some number theory which I was not able to
%% figure out myself. The code below is assembled from Python snippets
%% mostly from the day 22 reddit solution megathread.
part2(Instrs) ->
    N = 101741582076661, %% Reps
    D = 119315717514047, %% Deck size
    X = 2020,            %% The position we are interested in

    %% Run the shuffling backwards twice to get Y and Z.
    RInstrs = lists:reverse(Instrs),
    Y = shuffle2(X, RInstrs, D),
    Z = shuffle2(Y, RInstrs, D),

    %% Apply number theory to compute what card eventually ends up in
    %% position X.
    A = mod((Y - Z) * modinv(X - Y + D, D), D),
    B = mod(Y - A * X, D),
    (powmod(A, N, D) * X + (powmod(A, N, D) - 1) * modinv(A - 1, D) * B) rem D.

%% Apply the shuffling rules to the card at position X.
shuffle2(X, [], _) ->
    X;
shuffle2(X, [R | RInstrs], D) ->
    case R of
        {cut, Cut} ->
            shuffle2((X + Cut + D) rem D, RInstrs, D);
        {incr, Incr} ->
            shuffle2(modinv(Incr, D) * X, RInstrs, D);
        new ->
            shuffle2(D - 1 - X, RInstrs, D)
    end.

%% Remainder which handles negative numbers "correctly".
mod(X, Y) ->
    mod0(X rem Y, Y).

mod0(M, _) when M >= 0 ->
    M;
mod0(M, Y) ->
    mod0(M + Y, Y).

%% Modular multiplicative inverse from
%% https://stackoverflow.com/a/9758173/13051
egcd(0, B) ->
    {B, 0, 1};
egcd(A, B) ->
    {G, Y, X} = egcd(mod(B, A), A),
    {G, X - B div A * Y, Y}.

modinv(A, M) ->
    {G, X, _} = egcd(A, M),
    case G of
        -1 ->
            throw(mod_inv_does_not_exist);
        _ ->
            mod(X, M)
    end.

%% Fast modular exponentiation from
%% https://gist.github.com/Radcliffe/e41b41a441deda19e7ac5731197f49be
powmod(A, B, M) ->
    powmod(A, B, M, 1).

powmod(_, 0, _, R) ->
    R;
powmod(A, B, M, R) when B rem 2 == 1 ->
    powmod(A, B - 1, M, A * R rem M);
powmod(A, B, M, R) ->
    powmod(A * A rem M, B div 2, M, R).
