%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(aoc2020_day09).
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
             , day = 9
             , name = "Encoding Error"
             , expected = {138879426, 23761694}
             , has_input_file = true
             }.

%%==============================================================================
%% Types
%%==============================================================================
-type input_type() :: [integer()].
-type result1_type() :: any().
-type result2_type() :: result1_type().

%%------------------------------------------------------------------------------
%% @doc parse/1
%% Parses input file.
%% @end
%%------------------------------------------------------------------------------
-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  lists:map(fun erlang:list_to_integer/1,
            string:tokens(binary_to_list(Input), "\n\r")).

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
  find_first_invalid(Input, 25).

%%------------------------------------------------------------------------------
%% @doc solve2/1
%% Solves part 2. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve2(Prog :: input_type()) -> result2_type().
solve2(Input) ->
  find_range(Input, 138879426).

%%==============================================================================
%% Internals
%%==============================================================================

find_first_invalid([_|Rest] = List, N) ->
  {Preamble, [Next|_]} = lists:split(N, List),
  case [Next || X <- Preamble,
                Y <- Preamble,
                X /= Y, X + Y == Next] of
    [_|_] ->
      find_first_invalid(Rest, N);
    _ ->
      Next
  end.

find_range(List, Num) ->
  find_range(List, 2, Num).

find_range(List, Len, Num) when Len < length(List) ->
  case find_range0(List, Len, Num) of
    N when is_integer(N) ->
      N;
    false ->
      %% No range of len Num was found, try a longer one
      find_range(List, Len + 1, Num)
  end.

%% Find a range in `List' of length `Len' which sums up to `Num`.
%% Returns the sum of the first and last numbers if found, or false if
%% no such range was found.
find_range0(List, Len, _Num) when length(List) < Len ->
  false;
find_range0([_|Rest] = List, Len, Num) ->
  {Range, _} = lists:split(Len, List),
  Sum = lists:sum(Range),
  if Sum == Num ->
      lists:min(Range) + lists:max(Range);
     true ->
      find_range0(Rest, Len, Num)
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
