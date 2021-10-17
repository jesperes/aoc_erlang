%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(aoc2020_day22).

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
             , day = 22
             , name = "Crab Combat"
             , expected = {32677, 33661}
             , has_input_file = false
             }.

%%==============================================================================
%% Types
%%==============================================================================
-type input_type() :: {[integer()], [integer()]}.
-type result1_type() :: integer().
-type result2_type() :: result1_type().

%%------------------------------------------------------------------------------
%% @doc parse/1
%% Parses input file.
%% @end
%%------------------------------------------------------------------------------
-spec parse(_) -> input_type().
parse(_) ->
  {[ 29, 21, 38, 30, 25, 7, 2, 36, 16, 44, 20, 12, 45,
     4, 31, 34, 33, 42, 50, 14, 39, 37, 11, 43, 18],
   [ 32, 24, 10, 41, 13, 3, 6, 5, 9, 8, 48, 49, 46, 17,
     22, 35, 1, 19, 23, 28, 40, 26, 47, 15, 27]}.

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(Input :: input_type()) -> result1_type().
solve1({Deck1, Deck2}) ->
  crab_combat(Deck1, Deck2).

%%------------------------------------------------------------------------------
%% @doc solve2/1
%% Solves part 2. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve2(Input :: input_type()) -> result2_type().
solve2({Deck1, Deck2}) ->
  {_, Result} = rec_crab_combat(Deck1, Deck2),
  Result.

%% =======================================================================
%% Regular crab combat
%% =======================================================================

crab_combat([], Deck2) ->
  sum_deck(Deck2);
crab_combat(Deck1, []) ->
  sum_deck(Deck1);
crab_combat([D1|Deck1], [D2|Deck2]) when D1 > D2 ->
  crab_combat(Deck1 ++ [D1, D2], Deck2);
crab_combat([D1|Deck1], [D2|Deck2]) when D1 < D2 ->
  crab_combat(Deck1, Deck2 ++ [D2, D1]).

%% =======================================================================
%% Recursive crab combat
%% =======================================================================

rec_crab_combat(Deck1, Deck2) ->
  rec_crab_combat(Deck1, Deck2, sets:new()).

rec_crab_combat(Deck1, [], _Rounds) ->
  {player1, sum_deck(Deck1)};
rec_crab_combat([], Deck2, _Rounds) ->
  {player2, sum_deck(Deck2)};
rec_crab_combat(Deck1, Deck2, Rounds) ->
  State = {Deck1, Deck2},
  case sets:is_element(State, Rounds) of
    true ->
      {player1, sum_deck(Deck1)};
    false ->
      %% Remember new configuration
      Rounds0 = sets:add_element(State, Rounds),

      %% Draw cards
      [Draw1|Rest1] = Deck1,
      [Draw2|Rest2] = Deck2,

      %% If both players have at least as many cards remaining in
      %% their deck as the value of the card they just drew, the
      %% winner of the round is determined by playing a new game of
      %% Recursive Combat.
      case (Draw1 =< length(Rest1)) andalso (Draw2 =< length(Rest2)) of
        true ->
          %% The sub-game does *not* use the entire deck, but only the
          %% number specified by the drawn card...
          {L1, _} = lists:split(Draw1, Rest1),
          {L2, _} = lists:split(Draw2, Rest2),

          %% Play recursive game
          case rec_crab_combat(L1, L2, sets:new()) of
            {player1, _} ->
              rec_crab_combat(Rest1 ++ [Draw1, Draw2], Rest2, Rounds0);
            {player2, _} ->
              rec_crab_combat(Rest1, Rest2 ++ [Draw2, Draw1], Rounds0)
          end;

        false ->
          if Draw1 > Draw2 ->
              rec_crab_combat(Rest1 ++ [Draw1, Draw2], Rest2, Rounds0);
             Draw1 < Draw2 ->
              rec_crab_combat(Rest1, Rest2 ++ [Draw2, Draw1], Rounds0)
          end
      end
  end.

sum_deck(Deck) ->
  Len = length(Deck),
  lists:foldl(fun({N, D}, Acc) ->
                  N * D + Acc
              end, 0, lists:zip(lists:seq(Len, 1, -1), Deck)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
