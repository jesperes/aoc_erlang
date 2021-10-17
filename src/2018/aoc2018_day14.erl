-module(aoc2018_day14).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2018,
                day = 14,
                name = "Chocolate Carts",
                expected = {"5115114101", 20310465},
                has_input_file = false}.

-type input_type() :: integer().
-type result1_type() :: string().
-type result2_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    633601.

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    start1(Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    start2(integer_to_list(Input)).

%% Chocolate Carts

start1(Input) ->
  L = <<3, 7>>,
  Bin = step1(L, 0, 1, Input),
  lists:map(fun(X) -> X + $0 end, binary_to_list(Bin)).

step1(L, _, _, Input) when byte_size(L) >= Input + 10 ->
  binary:part(L, Input, 10);
step1(L, Elf1, Elf2, Input) ->
  {L0, NewElf1, NewElf2, _} = append_and_move_elves(L, Elf1, Elf2, Input, false),
  step1(L0, NewElf1, NewElf2, Input).

start2(InputStr) ->
  Bin = list_to_binary(lists:map(fun(X) -> X - $0 end, InputStr)),
  L = <<3, 7>>,
  step2(L, 0, 1, Bin, 0).

%% We know the answer is ~20 million, so stop at 21.
step2(L, _, _, _, _) when byte_size(L) > 21000000 -> false;
step2(_, _, _, _, {found, At}) -> At;
step2(L, Elf1, Elf2, Input, P) ->
  {L0, NewElf1, NewElf2, P0} = append_and_move_elves(L, Elf1, Elf2, Input, P),
  step2(L0, NewElf1, NewElf2, Input, P0).

%%% Helpers

append_and_move_elves(L, Elf1, Elf2, Input, P) ->
  E1 = binary:at(L, Elf1),
  E2 = binary:at(L, Elf2),
  LLen = byte_size(L),

  E12 = E1 + E2,
  {NewP, L0} =
    if E12 < 10 ->
        P1 = progress(P, E12, Input, LLen),
        {P1, <<L/binary, E12>>};
       true ->
        X = 1,
        Y = E12 rem 10,
        P1 = progress(P, X, Input, LLen),
        P2 = progress(P1, Y, Input, LLen + 1),
        {P2, <<L/binary, X, Y>>}
    end,
  Len = byte_size(L0),
  NewElf1 = (Elf1 + E1 + 1) rem Len,
  NewElf2 = (Elf2 + E2 + 1) rem Len,
  {L0, NewElf1, NewElf2, NewP}.

progress(false, _, _, _) -> false;
progress(P, _, _, _) when is_tuple(P) -> P;
progress(P, X, Input, I) when is_integer(P) ->
  NewP = case binary:at(Input, P) == X of
           true -> P + 1;
           false -> 0
         end,
  case NewP of
    L when L == byte_size(Input) ->
      {found, I - byte_size(Input) + 1};
    _ ->
      NewP
  end.
