%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(aoc2020_day23).

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
             , day = 23
             , name = "Crab Cups"
             , expected = {"28946753", 519044017360}
             , has_input_file = false
             }.

%%==============================================================================
%% Types
%%==============================================================================
-type input_type() :: [integer()].
-type result1_type() :: string().
-type result2_type() :: integer().

%%------------------------------------------------------------------------------
%% @doc parse/1
%% Parses input file.
%% @end
%%------------------------------------------------------------------------------
-spec parse(_) -> input_type().
parse(_) ->
  [5, 8, 6, 4, 3, 9, 1, 7, 2].

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
  solve(Input, 100).

%%------------------------------------------------------------------------------
%% @doc solve2/1
%% Solves part 2. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
  solve_extended(Input).

%%==============================================================================
%% Helpers
%%==============================================================================

solve([First|_] = Input, Moves) ->
  init(Input),
  do_moves(First, Moves),
  lists:map(fun(C) -> C + $0 end, tl(to_list(1))).

-define(ONE_MILLION, 1000000).
-define(TEN_MILLION, 10000000).

solve_extended(Input) ->
  solve_extended(Input, ?ONE_MILLION, ?TEN_MILLION).

solve_extended([First|_] = Input, FillTo, Moves) ->
  init(Input),
  fill_to(lists:last(Input), get(max) + 1, FillTo, First),
  put(max, FillTo),
  do_moves(First, Moves),
  A = get(1),
  B = get(A),
  A * B.

fill_to(Prev, Next, FillTo, First) when Next == FillTo ->
  put(Prev, Next),
  put(Next, First);
fill_to(Prev, Next, FillTo, First) ->
  put(Prev, Next),
  fill_to(Next, Next + 1, FillTo, First).

%% @doc Initialize the ring, using the process dictionary (CupNum ->
%% NextCup).
init([First|_] = L) ->
  erase(),
  put(max, lists:max(L)),
  init(L, First).

init([Last], First) ->
  put(Last, First);
init([A,B|L], F) ->
  put(A, B),
  init([B|L], F).

%% Convert the ring to a list, starting at `First'
to_list(First) ->
  to_list(First, get(First), [First]).

to_list(First, Current, Acc) when First == Current ->
  lists:reverse(Acc);
to_list(First, Current, Acc) ->
  to_list(First, get(Current), [Current|Acc]).

do_moves(Current, 0) ->
  Current;
do_moves(Current, N) ->
  A = get(Current),
  B = get(A),
  C = get(B),
  Next = get(C),
  erase(A),
  erase(B),
  erase(C),
  put(Current, Next),
  Dest = find_dest(Current),
  DestNext = get(Dest),
  put(Dest, A),
  put(A, B),
  put(B, C),
  put(C, DestNext),
  do_moves(Next, N - 1).

%% Find the destination cup.
find_dest(Current) ->
  Max = get(max),
  Dest = if Current == 1 -> Max; %% wrap around
            true -> Current - 1
         end,
  case get(Dest) of
    undefined -> find_dest(Dest);
    N when is_integer(N) -> Dest
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
