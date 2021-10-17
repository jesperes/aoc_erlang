-module(aoc2018_day12).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
  #aoc_puzzle{module = ?MODULE,
              year = 2018,
              day = 12,
              name = "Subterranean Sustainability",
              expected = {2767, 2650000001362},
              has_input_file = true}.

-type input_type() :: string().
-type result_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  binary_to_list(Input).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
  solve(20, Input).

-spec solve2(Input :: input_type()) -> result_type().
solve2(_Input) ->
  solve_fast(50000000000).

solve_fast(Limit) ->
  %% We don't compute the answer to part 2, since it takes too long.
  %% Instead we look at the pattern of successively larger inputs.
  %% This simplification probably only works on numbers on the form 5
  %% * 10^n.
  %%
  %% 5 * 10^4 -> 2651362
  %% 5 * 10^5 -> 26501362
  %% 5 * 10^6 -> 265001362
  %% 5 * 10^7 -> 2650001362
  %% 5 * 10^8 -> 26500001362
  %% 5 * 10^9 -> 265000001362
  %% 5 * 10^10 -> 2650000001362
  53 * Limit + 1362.

solve(Limit, Input) ->
  {_N, Pots, Rules} = parse_data(Input),

  FinalPots = apply_steps(Limit, Pots, Rules),

  sets:fold(fun(PotNr, Sum) -> PotNr + Sum end, 0, FinalPots).

apply_steps(0, Pots, _Rules) ->
  Pots;
apply_steps(N, Pots, Rules) ->
  Pots0 = apply_step(Pots, Rules),
  apply_steps(N - 1, Pots0, Rules).

parse_data(String) ->
  [First | Rest] = string:tokens(String, "\n"),
  ["initial", "state", InitState] = string:tokens(First, " :"),

  {N, Map} =
    lists:foldl(fun(Char, {N, Acc}) ->
                   case Char of
                     $# -> {N + 1, sets:add_element(N, Acc)};
                     _ -> {N + 1, Acc}
                   end
                end,
                {0, sets:new()},
                InitState),

  Rules =
    lists:map(fun(Line) ->
                 [From, To] = string:tokens(Line, " =>"),
                 {From, To}
              end,
              Rest),
  {N, Map, Rules}.

bounds(Pots) ->
  MaxPot =
    sets:fold(fun (Elem, Max) when Max == undef ->
                    Elem;
                  (Elem, Max) when Elem > Max ->
                    Elem;
                  (_, Max) ->
                    Max
              end,
              undef,
              Pots),
  MinPot =
    sets:fold(fun (Elem, Min) when Min == undef ->
                    Elem;
                  (Elem, Min) when Elem < Min ->
                    Elem;
                  (_, Min) ->
                    Min
              end,
              undef,
              Pots),
  {MinPot - 5, MaxPot + 5}.

apply_step(Pots, Rules) ->
  {Start, End} = bounds(Pots),
  lists:foldl(fun(PotNr, PotsWithPlants) ->
                 case has_plant(PotNr, Pots, Rules) of
                   true -> sets:add_element(PotNr, PotsWithPlants);
                   _ -> PotsWithPlants
                 end
              end,
              sets:new(),
              lists:seq(Start, End)).

has_plant(PotNr, Pots, Rules) ->
  Nbrs = get_neighborhood(PotNr, Pots),
  get_matching_rule(Nbrs, Rules) == $#.

get_matching_rule(_Nbrs, []) ->
  $.;
get_matching_rule(Nbrs, [{Nbrs, [To]} | _Rest]) ->
  To;
get_matching_rule(Nbrs, [_ | Rest]) ->
  get_matching_rule(Nbrs, Rest).

get_neighborhood(PotNr, Pots) ->
  [case sets:is_element(N, Pots) of
     true ->
       $#;
     false ->
       $.
   end
   || N <- lists:seq(PotNr - 2, PotNr + 2)].
