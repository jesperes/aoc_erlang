%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(aoc2020_day14).

-behavior(aoc_puzzle).

-export([ parse/1
        , solve1/1
        , solve2/1
        , info/0
        ]).

-include("aoc_puzzle.hrl").

%%------------------------------------------------------------------------------
%% @doc info/0
%% Returns info about this puzzle.
%% @end
%%------------------------------------------------------------------------------
-spec info() -> aoc_puzzle().
info() ->
  #aoc_puzzle{ module = ?MODULE
             , year = 2020
             , day = 14
             , name = "Docking Data"
             , expected = {8471403462063, 2667858637669}
             , has_input_file = true
             }.

%%==============================================================================
%% Types
%%==============================================================================
-type write() :: { write
                  , Address :: integer()
                  , Value :: integer()
                  }.
-type writes() :: [write()].
-type mask_block() :: {Mask :: string(),
                       Writes :: writes()}.
-type mask_blocks() :: [mask_block()].
-type input_type() :: mask_blocks().
-type result1_type() :: any().
-type result2_type() :: result1_type().

%%------------------------------------------------------------------------------
%% @doc parse/1
%% Parses input file.
%% @end
%%------------------------------------------------------------------------------
-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  lists:filtermap(
    fun(Bin) ->
        [First|Rest] = re:split(Bin, "\n", [multiline]),
        Writes = parse_writes(filter_empty(Rest), []),
        {true, {binary_to_list(First), Writes}}
    end, filter_empty(re:split(Input, "mask = ", [multiline]))).

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(Masks :: input_type()) -> result1_type().
solve1(Masks) ->
  Memory = apply_masks(Masks, #{}),
  lists:sum(maps:values(Memory)).

%%------------------------------------------------------------------------------
%% @doc solve2/1
%% Solves part 2. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve2(Masks :: input_type()) -> result2_type().
solve2(Masks) ->
  Memory = apply_masks2(Masks, #{}),
  lists:sum(maps:values(Memory)).

%%==============================================================================
%% Internals
%%==============================================================================

%%==============================================================================
%% Part 1
%%==============================================================================

-spec parse_writes(Lines :: [binary()], Acc :: writes()) -> writes().
parse_writes([], Writes) ->
  lists:reverse(Writes);
parse_writes([Write|Rest], Writes) ->
  [<<"mem">>, Addr, Value] = string:lexemes(Write, "[] = "),
  P = {write,
       binary_to_integer(Addr),
       binary_to_integer(Value)},
  parse_writes(Rest, [P|Writes]).

%% TODO rewrite these using foldl
apply_masks([], Memory) ->
  Memory;
apply_masks([{Mask, Writes}|Rest], Memory) ->
  Memory0 = apply_writes(Mask, Writes, Memory),
  apply_masks(Rest, Memory0).

apply_writes(_Mask, [], Memory) ->
  Memory;
apply_writes(Mask, [Write|Rest], Memory) ->
  Memory0 = apply_write(Mask, Write, Memory),
  apply_writes(Mask, Rest, Memory0).

apply_write(Mask, {write, Addr, Value}, Memory) ->
  NewValue = write_bits(Addr, Value, 35, Mask),
  maps:put(Addr, NewValue, Memory).

write_bits(_Addr, Value, _Pos, []) ->
  Value;
write_bits(Addr, Value, Pos, [MaskBit|Rest]) ->
  Value0 =
    case MaskBit of
      $0 -> Value band (bnot (1 bsl Pos));
      $1 -> Value bor (1 bsl Pos);
      $X -> Value
    end,
  write_bits(Addr, Value0, Pos - 1, Rest).

%%==============================================================================
%% Part 2
%%==============================================================================
apply_masks2(Masks, Memory) ->
  lists:foldl(
    fun({Mask, Writes}, MemoryIn) ->
        %% Find which bits are floating
        FloatingBits =
          lists:filtermap(
            fun({Pos, $X}) -> {true, Pos};
               (_) -> false
            end,
            lists:zip(lists:seq(35, 0, -1), Mask)),

        lists:foldl(
          fun({write, Addr, Value}, MemoryIn0) ->
              %% Apply the mask for non-floating bits
              AppliedMask =
                lists:foldl(
                  fun({Pos, $1}, N) -> N bor (1 bsl Pos);
                     (_, N) -> N
                  end, Addr, lists:zip(lists:seq(35, 0, -1), Mask)),

              %% Expand floating writes. The number of writes will be
              %% 2 ** (number of X:s).
              ExpandedWrites = expand_writes(FloatingBits, AppliedMask, Value),

              %% Apply the floating writes
              lists:foldl(fun({write, A, V}, Acc) ->
                              maps:put(A, V, Acc)
                          end, MemoryIn0, ExpandedWrites)
          end, MemoryIn, Writes)
    end, Memory, Masks).

expand_writes(Bits, Addr, Value) ->
  lists:sort(lists:map(fun(N) ->
                           {write, make_expanded_mask(Addr, Bits, N), Value}
                       end, lists:seq(0, 1 bsl (length(Bits)) - 1))).

%% `Addr' is the raw address with the mask (except floating bits)
%% applied. `Bits' are the bit numbers of the floating bits, and `N' is
%% the bitpattern to use.
%%
%% If `Bits` is [3,1,0] and `N' is 011, we should write, into Addr, a
%% 0 at position 3, a 1 at position 1, and a 0 at position 0.
make_expanded_mask(Addr, Bits, N) ->
  lists:foldl(fun({I, Pos}, Acc) ->
                  case (N bsr I) band 1 of
                    1 -> Acc bor (1 bsl Pos);
                    0 -> Acc band (bnot (1 bsl Pos))
                  end
              end, Addr, with_index(Bits)).

with_index(L) ->
  lists:zip(lists:seq(0, length(L) - 1), L).

-spec filter_empty([binary()]) -> [binary()].
filter_empty(L) ->
  lists:filter(fun(<<>>) -> false;
                  (_) -> true
               end, L).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
