-module(aoc2015_day13).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 13,
                name = "Knights of the Dinner Table",
                expected = {709, 668},
                has_input_file = true}.

-type input_type() :: [{atom(), integer(), atom()}].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    lists:map(fun(Line) ->
                 [A,
                  "would",
                  GainOrLose,
                  Amount,
                  "happiness",
                  "units",
                  "by",
                  "sitting",
                  "next",
                  "to",
                  B] =
                     string:tokens(Line, " ."),
                 case list_to_atom(GainOrLose) of
                     gain -> {list_to_atom(A), list_to_integer(Amount), list_to_atom(B)};
                     lose -> {list_to_atom(A), -list_to_integer(Amount), list_to_atom(B)}
                 end
              end,
              string:tokens(binary_to_list(Input), "\n\r")).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    run(false, Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    run(true, Input).

permute([]) ->
    [[]];
permute(L) ->
    [[X | Y] || X <- L, Y <- permute(L -- [X])].

-spec run(boolean(), input_type()) -> any().
run(IncludeSelf, Input) ->
    [First | _] =
        NameList =
            case IncludeSelf of
                true ->
                    sets:to_list(get_all_names(Input)) ++ ['Myself'];
                false ->
                    sets:to_list(get_all_names(Input))
            end,

    CostMap = name_list_to_map(Input, #{}),

    %% Filter out all permutations except the once beginning with the
    %% first name. This excludes a number of identical combinations.
    AllTables =
        lists:filter(fun([FirstName | _]) -> FirstName == First end, permute(NameList)),

    %% We can also cut the number of tables in half by filtering out
    %% duplicate reverse lists (i.e. traversing the table in the other
    %% direction.
    AllHappiness = [{Table, happiness(Table, CostMap)} || Table <- AllTables],

    lists:foldl(fun ({_Table, Score}, Max) when Score > Max ->
                        Score;
                    (_, Max) ->
                        Max
                end,
                0,
                AllHappiness).

name_list_to_map([], Map) ->
    Map;
name_list_to_map([{A, Amount, B} | Rest], Map) ->
    M0 = maps:put({A, B}, Amount, Map),
    name_list_to_map(Rest, M0).

get_all_names(List) ->
    get_all_names(List, sets:new()).

get_all_names([], Names) ->
    Names;
get_all_names([{A, _, B} | Rest], Names) ->
    N0 = sets:add_element(A, Names),
    N1 = sets:add_element(B, N0),
    get_all_names(Rest, N1).

happiness([First | _] = Table, CostMap) ->
    happiness(Table, First, CostMap).

happiness([A], First, CostMap) ->
    %% wrap around to first
    happiness0(A, First, CostMap);
happiness([A, B | Rest], First, CostMap) ->
    happiness0(A, B, CostMap) + happiness([B | Rest], First, CostMap).

happiness0(A, B, CostMap) ->
    maps:get({A, B}, CostMap, 0) + maps:get({B, A}, CostMap, 0).
