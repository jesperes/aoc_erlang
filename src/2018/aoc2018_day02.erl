-module(aoc2018_day02).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2018,
                day = 2,
                name = "Inventory Management System",
                expected = {9139, "uqcidadzwtnhsljvxyobmkfyr"},
                has_input_file = true}.

-type input_type() :: [string()].
-type result1_type() :: integer().
-type result2_type() :: string().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    string:tokens(binary_to_list(Input), "\n\r").

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    %% This is a tuple containing the number of strings which has a
    %% letter occurring twice and thrice, respectively.
    {X, Y} =
        lists:foldl(fun(Line, {Twos, Threes}) ->
                       FT = freq_table(Line, #{}),
                       {Twos + has_n(FT, 2), Threes + has_n(FT, 3)}
                    end,
                    {0, 0},
                    Input),
    X * Y.

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Lines) ->
    DiffList = [{X, Y, number_of_different_chars(X, Y)} || X <- Lines, Y <- Lines, X < Y],

    [Solution] =
        lists:filtermap(fun({X, Y, N}) ->
                           if N =:= 1 -> {true, remove_diff_char(X, Y)};
                              true -> false
                           end
                        end,
                        DiffList),
    Solution.

%% Build a frequency table of the given string
freq_table([], Table) ->
    Table;
freq_table([Char | Rest], Table0) ->
    Table = maps:update_with(Char, fun(N) -> N + 1 end, 1, Table0),
    freq_table(Rest, Table).

%% Returns 1 if the Table map contains the value N, 0 otherwise.
has_n(Table, N) ->
    case lists:member(N, maps:values(Table)) of
        true ->
            1;
        false ->
            0
    end.

number_of_different_chars([], []) ->
    0;
number_of_different_chars([X | Xs], [X | Ys]) ->
    0 + number_of_different_chars(Xs, Ys);
number_of_different_chars([_ | Xs], [_ | Ys]) ->
    1 + number_of_different_chars(Xs, Ys).

remove_diff_char([], []) ->
    [];
remove_diff_char([X | Xs], [X | Ys]) ->
    [X | remove_diff_char(Xs, Ys)];
remove_diff_char([_ | Xs], [_ | Ys]) ->
    remove_diff_char(Xs, Ys).
