-module(aoc2015_day15).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 15,
                name = "Science for Hungry People",
                expected = {222870, 117936},
                has_input_file = true}.

-type input_type() :: map().
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    lists:foldl(fun(X, Map) ->
                   [Ingredient,
                    "capacity",
                    Cap,
                    "durability",
                    Dur,
                    "flavor",
                    Flavor,
                    "texture",
                    Texture,
                    "calories",
                    Calories] =
                       string:tokens(X, ":, "),
                   Map#{list_to_atom(Ingredient) =>
                            #{capacity => list_to_integer(Cap),
                              durability => list_to_integer(Dur),
                              flavor => list_to_integer(Flavor),
                              texture => list_to_integer(Texture),
                              calories => list_to_integer(Calories)}}
                end,
                #{},
                string:tokens(binary_to_list(Input), "\n\r")).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Ingredients) ->
    Combinations =
        [[X1, X2, X3, X4]
         || X1 <- lists:seq(1, 100),
            X2 <- lists:seq(1, 100),
            X3 <- lists:seq(1, 100),
            X4 <- lists:seq(1, 100),
            X1 + X2 + X3 + X4 == 100],

    lists:max(
        lists:map(fun(Amounts) -> score(Amounts, Ingredients) end, Combinations)).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Ingredients) ->
    Combinations =
        [[X1, X2, X3, X4]
         || X1 <- lists:seq(1, 100),
            X2 <- lists:seq(1, 100),
            X3 <- lists:seq(1, 100),
            X4 <- lists:seq(1, 100),
            X1 + X2 + X3 + X4 == 100],

    lists:max(
        lists:filtermap(fun ({Score, Cal}) when Cal == 500 ->
                                {true, Score};
                            (_) ->
                                false
                        end,
                        lists:map(fun(Amounts) -> score2(Amounts, Ingredients) end, Combinations))).

properties() ->
    [capacity, durability, flavor, texture].

ingredient_amount([X1, _, _, _], 'Sugar') ->
    X1;
ingredient_amount([_, X2, _, _], 'Sprinkles') ->
    X2;
ingredient_amount([_, _, X3, _], 'Candy') ->
    X3;
ingredient_amount([_, _, _, X4], 'Chocolate') ->
    X4;
ingredient_amount([X1, _], 'Butterscotch') ->
    X1;
ingredient_amount([_, X2], 'Cinnamon') ->
    X2.

score(Amounts, Ingredients) ->
    %% Amounts is a list [X1,X2,X3,X4] where each X corresponds to an
    %% ingredient and indicates how much of each ingredient to take.
    lists:foldl(fun(Prop, Acc) ->
                   %% For each property (e.g. 'capacity'), multiply each
                   %% ingredient's 'capacity' value with the amount of that
                   %% ingredient
                   PropVal =
                       maps:fold(fun(Ingredient, IngredientMap, Acc0) ->
                                    A = ingredient_amount(Amounts, Ingredient),
                                    V = maps:get(Prop, IngredientMap),
                                    Acc0 + A * V
                                 end,
                                 0,
                                 Ingredients),

                   if PropVal < 0 -> 0;
                      true -> PropVal * Acc
                   end
                end,
                1,
                properties()).

score2(Amounts, Ingredients) ->
    %% Amounts is a list [X1,X2,X3,X4] where each X corresponds to an
    %% ingredient and indicates how much of each ingredient to take.
    Score =
        lists:foldl(fun(Prop, Acc) ->
                       %% For each property (e.g. 'capacity'), multiply each
                       %% ingredient's 'capacity' value with the amount of that
                       %% ingredient
                       PropVal =
                           maps:fold(fun(Ingredient, IngredientMap, Acc0) ->
                                        A = ingredient_amount(Amounts, Ingredient),
                                        V = maps:get(Prop, IngredientMap),
                                        Acc0 + A * V
                                     end,
                                     0,
                                     Ingredients),

                       if PropVal < 0 -> 0;
                          true -> PropVal * Acc
                       end
                    end,
                    1,
                    properties()),

    Calories =
        maps:fold(fun(Ingredient, IngredientMap, Acc0) ->
                     A = ingredient_amount(Amounts, Ingredient),
                     V = maps:get(calories, IngredientMap),
                     Acc0 + A * V
                  end,
                  0,
                  Ingredients),
    {Score, Calories}.
