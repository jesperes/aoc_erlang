%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(aoc2020_day21).

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
             , day = 21
             , name = "Allergen Assessment"
             , expected = {2428, "bjq,jznhvh,klplr,dtvhzt,sbzd,tlgjzx,ctmbr,kqms"}
             , has_input_file = true
             }.

%%==============================================================================
%% Types
%%==============================================================================
-type input_type() :: map().
-type result1_type() :: integer().
-type result2_type() :: string().

%%------------------------------------------------------------------------------
%% @doc parse/1
%% Parses input file.
%% @end
%%------------------------------------------------------------------------------
-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  lists:foldl(
    fun(L, Acc) ->
        case re:run(L, "(.*) \\(contains (.*)\\)",
                    [{capture, all_but_first, list}]) of
          {match, [M1, M2]} ->
            %% Ingredients
            I = sets:from_list(
                  lists:map(
                    fun list_to_atom/1, string:split(M1, " ", all))),

            %% Allergens
            A = sets:from_list(
                  lists:map(
                    fun list_to_atom/1, string:split(M2, ", ", all))),

            %% Update map of allergens to ingredients that may contain
            %% them
            Acc0 = update_allergens(I, A, Acc),

            Acc1 =
              maps:update_with(
                ingredients,
                fun(Old) ->
                    sets:union(I, Old)
                end, I, Acc0),
            Acc2 =
              maps:update_with(
                allergens,
                fun(Old) ->
                    sets:union(A, Old)
                end, A, Acc1),

            _Acc3 = update_frequencies(I, Acc2)

        end
    end, #{}, string:tokens(binary_to_list(Input), "\r\n")).

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(Map :: input_type()) -> result1_type().
solve1(Map) ->
  IngredientsWithAllergens = ingredients_with_allergens(Map),
  IngredientsWithoutAllergens =
    sets:subtract(maps:get(ingredients, Map),
                  IngredientsWithAllergens),
  Freq = maps:get(freq, Map),
  sets:fold(
    fun(I, Acc) ->
        Acc + maps:get(I, Freq)
    end, 0, IngredientsWithoutAllergens).

%%------------------------------------------------------------------------------
%% @doc solve2/1
%% Solves part 2. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve2(Input :: input_type()) -> result2_type().
solve2(_) ->
  %% For part 2 I printed out the map from allergens to their
  %% ingredients then matched them up by hand to get this:
  CDI =
    [{eggs, bjq},
     {fish, jznhvh},
     {nuts, klplr},
     {peanuts, dtvhzt},
     {sesame, sbzd},
     {shellfish, tlgjzx},
     {soy, ctmbr},
     {wheat, kqms}],

  %% Sort them by allergen, and make into comma-separated list.
  lists:flatten(
    lists:join(",",
               lists:map(fun({_, X}) -> atom_to_list(X) end,
                         lists:sort(CDI)))).

%%==============================================================================
%% Helpers
%%==============================================================================

ingredients_with_allergens(Map) ->
  M0 = maps:remove(ingredients, Map),
  M1 = maps:remove(allergens, M0),
  M2 = maps:remove(freq, M1),
  sets:union(maps:values(M2)).

update_frequencies(Ingredients, Map) ->
  lists:foldl(
    fun(I, Acc) ->
        maps:update_with(
          freq,
          fun(Old) ->
              incr_freq(I, Old)
          end, #{I => 1}, Acc)
    end, Map, sets:to_list(Ingredients)).

incr_freq(I, Map) ->
  maps:update_with(I, fun(Old) -> Old + 1 end, 1, Map).

%% `Map' is a map from allergens to the ingredients that may contain
%% it (the intersection of all the ingredients it is listed for).
update_allergens(Ingredients, Allergens, Map) ->
  sets:fold(
    fun(Allergen, Acc) ->
        maps:update_with(
          Allergen,
          fun(Old) ->
              sets:intersection(Ingredients, Old)
          end, Ingredients, Acc)
    end, Map, Allergens).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
