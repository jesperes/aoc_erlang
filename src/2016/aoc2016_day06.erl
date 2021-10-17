-module(aoc2016_day06).

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 6,
                name = "Signals and Noise",
                expected = {"dzqckwsd", "lragovly"},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: any().
-type result_type() :: {any(), any()}.

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    string:tokens(binary_to_list(Input), "\n\r").

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    get_message(Input).

find_keys(Value, Map) when is_map(Map) ->
    maps:keys(
        maps:filter(fun(_, V) -> Value =:= V end, Map)).

get_maxmin_word(Words, Pos) ->
    FM = lists:foldl(fun(Word, Acc) ->
                        CharAtPos = lists:nth(Pos, Word),
                        maps:update_with(CharAtPos, fun(Old) -> Old + 1 end, 1, Acc)
                     end,
                     #{},
                     Words),
    Freqs = maps:values(FM),
    %% Find the char corresponding to the minimum/maximum frequencies.
    %% There should only be one each of these.
    [MinChar] = find_keys(lists:min(Freqs), FM),
    [MaxChar] = find_keys(lists:max(Freqs), FM),
    {MaxChar, MinChar}.

get_message([First | _] = Words) ->
    lists:unzip(
        lists:map(fun(Pos) -> get_maxmin_word(Words, Pos) end, lists:seq(1, length(First)))).
