-module(aoc2015_day09).

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 9,
                name = "All in a Single Night",
                expected = {251, 898},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: [{atom(), atom(), integer()}].
-type result_type() :: {any(), any()}.

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    lists:map(fun(Line) ->
                 [From, "to", To, "=", Dist] = string:tokens(Line, " "),
                 {list_to_atom(From), list_to_atom(To), list_to_integer(Dist)}
              end,
              string:tokens(binary_to_list(Input), "\n\r")).

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    {Set, Map} = get_cities(Input),
    Cities = sets:to_list(Set),
    Distances = [distance(CityList, Map) || CityList <- permute(Cities)],
    {lists:min(Distances), lists:max(Distances)}.

permute([]) ->
    [[]];
permute(L) ->
    [[X | Y] || X <- L, Y <- permute(L -- [X])].

city_pair(C1, C2) when C1 < C2 ->
    {C1, C2};
city_pair(C1, C2) ->
    {C2, C1}.

get_cities(Cities) ->
    get_cities(Cities, {sets:new(), maps:new()}).

get_cities([], Cities) ->
    Cities;
get_cities([{City1, City2, Dist} | Rest], {Set, Map}) ->
    S0 = sets:add_element(City1, Set),
    S1 = sets:add_element(City2, S0),
    M0 = maps:put(city_pair(City1, City2), Dist, Map),
    get_cities(Rest, {S1, M0}).

distance([], _Map) ->
    0;
distance([_], _Map) ->
    0;
distance([A, B | Rest], Map) ->
    maps:get(city_pair(A, B), Map) + distance([B | Rest], Map).
