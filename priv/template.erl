%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(template).

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
             , day = 1
             , name = "..."
             , expected = {none, none}
             , has_input_file = true
             }.

%%==============================================================================
%% Types
%%==============================================================================
-type input_type() :: binary().
-type result1_type() :: integer().
-type result2_type() :: result1_type().

%%------------------------------------------------------------------------------
%% @doc parse/1
%% Parses input file.
%% @end
%%------------------------------------------------------------------------------
-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  Input.

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(Input :: input_type()) -> result1_type().
solve1(_Input) ->
  throw(not_implemented).

%%------------------------------------------------------------------------------
%% @doc solve2/1
%% Solves part 2. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve2(Input :: input_type()) -> result2_type().
solve2(_Input) ->
  throw(not_implemented).

%%==============================================================================
%% Helpers
%%==============================================================================

%%==============================================================================
%% Unit tests
%%==============================================================================

-ifdef(EUNIT).

ex1_test_() ->
  ?_assert(remove_if_unused).

-endif.

%%% === Emacs ===
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
