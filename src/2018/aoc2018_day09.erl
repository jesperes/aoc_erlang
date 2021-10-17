-module(aoc2018_day09).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2018,
                day = 9,
                name = "Marble Mania",
                expected = {423717, 3553108197},
                has_input_file = false}.

-type input_type() :: {integer(), integer()}.
-type result_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
  {419, 72164}.

-spec solve1(Input :: input_type()) -> result_type().
solve1({Players, LastMarble}) ->
  start(Players, LastMarble).

-spec solve2(Input :: input_type()) -> result_type().
solve2({Players, LastMarble}) ->
  start(Players, LastMarble * 100).

-spec start(integer(), integer()) -> integer().
start(NP, Last) ->
  {_, Scores} = marble_game(1, ring_insert(0, ring_new()),
                            NP, #{}, Last),
  lists:max(maps:values(Scores)).

marble_game(Marble, Ring, _, Scores, Last) when Marble > Last ->
  {Ring, Scores};
marble_game(Marble, Ring, NP, Scores, Last) when Marble rem 23 == 0 ->
  R0 = lists:foldl(fun(_N, Acc) ->
                       ring_rotate_cw(Acc)
                   end, Ring, lists:seq(1, 7)),
  {M, R1} = ring_remove(R0),
  Score = Marble + M,
  S0 = maps:update_with(Marble rem NP, fun(V) -> V + Score end, Score, Scores),
  marble_game(Marble + 1, R1, NP, S0, Last);

marble_game(Marble, Ring, NP, Scores, Last) ->
  R1 = ring_rotate_ccw(Ring),
  R2 = ring_rotate_ccw(R1),
  R3 = ring_insert(Marble, R2),
  marble_game(Marble + 1, R3, NP, Scores, Last).

%%% Ring data type

ring_new() ->
  {[], []}.

%% Insert N at the current position.
ring_insert(N, {CCW, CW}) ->
  {CCW, [N|CW]}.

%% Remove N from current position.
ring_remove({CCW, [C|CW]}) ->
  {C, {CCW, CW}}.

%% Rotate the ring counter-clockwise one step.
ring_rotate_ccw({CCW, [C|CW]}) ->
  {[C|CCW], CW};
ring_rotate_ccw({CCW, []}) ->
  {NewCCW, NewCW} = lists:split(length(CCW) div 2, CCW),
  ring_rotate_ccw({NewCCW, lists:reverse(NewCW)}).

%% Rotate the ring clockwise one step.
ring_rotate_cw({[C|CCW], CW}) ->
  {CCW, [C|CW]};
ring_rotate_cw({[], CW}) ->
  {NewCCW, NewCW} = lists:split(length(CW) div 2, CW),
  ring_rotate_cw({NewCCW, lists:reverse(NewCW)}).
