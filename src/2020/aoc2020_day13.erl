%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(aoc2020_day13).

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
             , day = 13
             , name = "Shuttle Search"
             , expected = {333, 690123192779524}
             , has_input_file = true
             }.

%%==============================================================================
%% Types
%%==============================================================================
-type input_type() :: [string()].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

%%------------------------------------------------------------------------------
%% @doc parse/1
%% Parses input file.
%% @end
%%------------------------------------------------------------------------------
-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  string:tokens(binary_to_list(Input), "\n\r").

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(Lines :: input_type()) -> result1_type().
solve1(Lines) ->
  [TimestampStr, DepartureStr] = Lines,
  TS = list_to_integer(TimestampStr),
  Deps = lists:map(fun list_to_integer/1,
                   re:split(DepartureStr, "[,x]+", [{return, list}])),
  find_next_dep(TS, Deps).

%%------------------------------------------------------------------------------
%% @doc solve2/1
%% Solves part 2. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve2(Lines :: input_type()) -> result2_type().
solve2(Lines) ->
  [_, DepartureStr] = Lines,
  L = re:split(DepartureStr, ",", [{return, list}]),
  Congruences =
    lists:filtermap(fun({_, "x"}) -> false;
                       ({A, N}) ->
                        Modulo = list_to_integer(N), % aka bus ID
                        Residue = Modulo - A,
                        {true, {Residue, Modulo}}
                    end,
                    lists:zip(lists:seq(0, length(L)-1), L)),
  chinese_remainder(Congruences).

%%==============================================================================
%% Internals
%%==============================================================================

find_next_dep(TS, Deps) ->
  [{T, ID}|_] =
    lists:sort(lists:map(fun(ID0) ->
                             {ID0 - (TS rem ID0), ID0}
                         end, Deps)),
  T * ID.

%%==============================================================================
%% CRT impl from https://rosettacode.org/wiki/Chinese_remainder_theorem#Erlang
%%==============================================================================

egcd(_, 0) -> {1, 0};
egcd(A, B) ->
  {S, T} = egcd(B, A rem B),
  {T, S - (A div B)*T}.

mod(A, M) ->
  X = A rem M,
  if X < 0 -> X + M;
     true -> X
  end.

mod_inv(A, B) ->
  {X, Y} = egcd(A, B),
  if
    A*X + B*Y =:= 1 -> X;
    true -> undefined
  end.

calc_inverses([], []) -> [];
calc_inverses([N | Ns], [M | Ms]) ->
  case mod_inv(N, M) of
    undefined -> undefined;
    Inv -> [Inv | calc_inverses(Ns, Ms)]
  end.

chinese_remainder(Congruences) ->
  {Residues, Modulii} = lists:unzip(Congruences),
  ModPI = lists:foldl(fun(A, B) -> A*B end, 1, Modulii),
  CRT_Modulii = [ModPI div M || M <- Modulii],
  case calc_inverses(CRT_Modulii, Modulii) of
    undefined -> undefined;
    Inverses ->
      Solution =
        lists:sum([A*B ||
                    {A,B} <-
                      lists:zip(
                        CRT_Modulii,
                        [A*B || {A,B} <- lists:zip(
                                           Residues,
                                           Inverses)
                        ])
                  ]),
      mod(Solution, ModPI)
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
