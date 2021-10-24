-module(aoc2017_day10).

-behavior(aoc_puzzle).

-export([ parse/1
        , solve1/1
        , solve2/1
        , info/0
        ]).

%% Used in later puzzles
-export([ knot_hash/1
        ]).

-include_lib("eunit/include/eunit.hrl").

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 10,
                name = "Knot Hash",
                expected = {38628, "e1462100a34221a7f0906da15c1c979a"},
                has_input_file = false}.

-type input_type() :: string().
-type result1_type() :: integer().
-type result2_type() :: string().

-spec parse(Binary :: binary()) -> input_type().
parse(_Binary) ->
    "130,126,1,11,140,2,255,207,18,254,246,164,29,104,0,224".

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    Lengths = lists:map(fun list_to_integer/1, string:tokens(Input, ",")),
    List = lists:seq(0, 255),
    {[A, B | _], _, _} = knot_hash(Lengths, List, 0, 0),
    A * B.

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    knot_hash(Input).

%% Helpers

-spec knot_hash(string()) -> string().
knot_hash(String) ->
    Suffix = [17, 31, 73, 47, 23],
    Lengths = String ++ Suffix,
    {SparseHash, _, _} =
        lists:foldl(fun(_, {List, Pos, SkipSize}) -> knot_hash(Lengths, List, Pos, SkipSize) end,
                    {lists:seq(0, 255), 0, 0},
                    lists:seq(1, 64)),
    hex_hash(dense_hash(SparseHash)).


knot_hash([L | Lengths], List, Pos, SkipSize) ->
    {L0, Pos0, SkipSize0} = knot_hash1(L, List, Pos, SkipSize),
    knot_hash(Lengths, L0, Pos0, SkipSize0);
knot_hash([], List, Pos, SkipSize) ->
    {List, Pos, SkipSize}.

%% This is not terribly efficient (lots of list copying), but the
%% puzzle runtime is still in the millisecond range.
knot_hash1(Length, List, Pos, SkipSize) ->
    ListLen = length(List),
    {A, B} = lists:split(Pos, List),
    L0 = B ++ A, % bring current position to head
    {A0, B0} = lists:split(Length, L0),
    L1 = lists:reverse(A0) ++ B0,
    {A1, B1} = lists:split(ListLen - Pos, L1),
    {B1 ++ A1, (Pos + Length + SkipSize) rem ListLen, SkipSize + 1}.

dense_hash([]) ->
    [];
dense_hash([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16
            | Rest]) ->
    [A1
     bxor A2
     bxor A3
     bxor A4
     bxor A5
     bxor A6
     bxor A7
     bxor A8
     bxor A9
     bxor A10
     bxor A11
     bxor A12
     bxor A13
     bxor A14
     bxor A15
     bxor A16
     | dense_hash(Rest)].

hex_hash(DenseHash) ->
    lists:flatten([io_lib:format("~2.16.0b", [N]) || N <- DenseHash]).
