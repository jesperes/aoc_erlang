%%% Advent of Code solution for 2020 day 07.
%%% Created: 2020-12-07T05:38:13+00:00

-module(aoc2020_day07).
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
             , day = 7
             , name = "Handy Haversacks"
             , expected = {355, 5312}
             , has_input_file = true
             }.

-type input_type() :: [string()].
-type result1_type() :: integer().
-type result2_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  string:tokens(binary_to_list(Input), "\n\r").

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
  part1(Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
  part2(Input).

part1(Input) ->
  Bags = parse_input1(Input),
  Reachable = digraph_utils:reachable(['shiny gold'], Bags),
  length(Reachable) - 1. %% exclude the 'shiny gold' bag itself

part2(Input) ->
  num_bags('shiny gold', parse_input2(Input)).

%% For part 2, we just do a simple depth-first traversal of (part of)
%% the graph.
num_bags(Bag, Bags) ->
  Contents = maps:get(Bag, Bags, []),
  lists:foldl(
    fun({N, Color}, Acc) ->
        N * (1 + num_bags(Color, Bags)) + Acc
    end, 0, Contents).

%% ===============================================================
%% Input parser
%% ===============================================================

%% TODO optimize solution by just doing this part once for both
%% solutions.
parse_input(Lines, Init, Fun) ->
  lists:foldl(
    fun(Line, Acc) ->
        [BagColor, Contents] = string:split(Line, " bags contain "),
        ContentList = lists:map(fun string:trim/1,
                                string:lexemes(Contents, ",.")),
        ColorList =
          lists:flatten(
            lists:map(
              fun(Content) ->
                  case string:lexemes(Content, " ") of
                    [N, C1, C2, _] ->
                      {list_to_integer(N), list_to_atom(C1 ++ " " ++ C2)};
                    ["no", "other", "bags"] ->
                      []
                  end
              end, ContentList)),

        Fun(list_to_atom(BagColor), ColorList, Acc)
    end, Init, Lines).

%% Parse input into a OTP digraph for part 1
parse_input1(Lines) ->
  Graph = digraph:new(),
  parse_input(
    Lines, ok,
    fun(BagColor, Colors, _) ->
        BagV = digraph:add_vertex(Graph, BagColor),
        lists:foreach(
          fun({_, C}) ->
              CV = digraph:add_vertex(Graph, C),
              digraph:add_edge(Graph, CV, BagV)
          end, Colors)
    end),
  Graph.

%% Parse input into a plain map for part 2.
parse_input2(Lines) ->
  parse_input(
    Lines, #{},
    fun(BagColor, Colors, Acc) ->
        lists:foldl(
          fun(C, InnerAcc) ->
              maps:update_with(
                BagColor, fun(Old) -> [C|Old] end,
                [C], InnerAcc)
          end, Acc, Colors)
    end).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
