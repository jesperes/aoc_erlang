-module(aoc2018_day08).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
  #aoc_puzzle{module = ?MODULE,
              year = 2018,
              day = 8,
              name = "Memory Maneuver",
              expected = {36307, 25154},
              has_input_file = true}.

-type input_type() :: [integer()].
-type result_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  lists:map(fun list_to_integer/1,
            string:tokens(
              string:trim(binary_to_list(Input)), " ")).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
  {Root, [], _} = parse_node(Input, 0),
  metadata_sum(Root).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
  {Root, [], _} = parse_node(Input, 0),
  metadata_sum2(Root).

%% Parse a single node. Returns a tuple {Node, Rest, NextId}.
parse_node([NumChildNodes, NumMetadataEntries | Rest], Id) ->
  {ChildNodes, Rem, NextId} = parse_child_nodes(NumChildNodes, [], Rest, Id + 1),
  {Metadata, Rem1} = lists:split(NumMetadataEntries, Rem),
  {{list_to_atom([$A + Id]), ChildNodes, Metadata}, Rem1, NextId}.

%% Parse N nodes from List
parse_child_nodes(0, Acc, List, Id) ->
  {lists:reverse(Acc), List, Id};
parse_child_nodes(N, Acc, List, Id) ->
  {ChildNode, Rest, NextId} = parse_node(List, Id),
  parse_child_nodes(N - 1, [ChildNode | Acc], Rest, NextId).

metadata_sum({_Id, ChildNodes, Metadata}) ->
  lists:sum(Metadata)
  + lists:foldl(fun(Node, Acc) -> Acc + metadata_sum(Node) end, 0, ChildNodes).

%% ------------------------------------------------------------
%% Part 2
%% ------------------------------------------------------------

metadata_sum2({_Id, [], Metadata}) ->
  lists:sum(Metadata);
metadata_sum2({Id, ChildNodes, Metadata}) ->
  sum_child_nodes(Id, Metadata, ChildNodes).

sum_child_nodes(_, [], _ChildNodes) ->
  0;
sum_child_nodes(Id, [M | Metadata], ChildNodes) ->
  child_index_sum(M, ChildNodes) + sum_child_nodes(Id, Metadata, ChildNodes).

child_index_sum(M, ChildNodes) ->
  try
    metadata_sum2(lists:nth(M, ChildNodes))
  catch
    _:_ ->
      0 %% invalid metadata entry
  end.
