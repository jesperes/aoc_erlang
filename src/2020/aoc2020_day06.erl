%%% Advent of Code solution for 2020 day 06.
%%% Created: 2020-12-06T06:25:52+00:00

-module(aoc2020_day06).
-behavior(aoc_puzzle).

-export([ parse/1
        , solve1/1
        , solve2/1
        , info/0
        ]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
  #aoc_puzzle{ module = ?MODULE
             , year = 2020
             , day = 6
             , name = "Custom Customs"
             , expected = {6680, 3117}
             , has_input_file = true
             }.

-type input_type() :: [string()].
-type result1_type() :: integer().
-type result2_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  string:split(string:trim(binary_to_list(Input)), "\n\n", all).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
  lists:sum(lists:map(fun num_yes_answers/1, Input)).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
  lists:sum(lists:map(fun all_yes_answers/1, Input)).

%% Returns the number of questions to which *anyone* answered yes.
%% This is simply the number of distinct letters in the input strings.
num_yes_answers(Group) ->
  length(lists:usort(
           lists:flatten(
             string:split(Group, "\n", all)))).

%% Returns the number of questions to which *everyone* answered
%% yes. This is the intersection of letters; i.e. the number of
%% letters which occurs in all answer strings.
all_yes_answers(Groups) ->
  [First|Rest] = string:split(Groups, "\n", all),
  sets:size(
    lists:foldl(fun(Group, Acc) ->
                    sets:intersection(Acc, sets:from_list(Group))
                end, sets:from_list(First), Rest)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
