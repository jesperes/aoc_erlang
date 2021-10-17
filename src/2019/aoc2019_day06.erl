%%% Advent of Code solution for 2019 day 06.
%%% Created: 2019-12-06T06:41:22+00:00

-module(aoc2019_day06).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 6,
                name = "Universal Orbit Map",
                expected = {300598, 520},
                has_input_file = true}.

-type input_type() :: [string()].
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    string:tokens(
        string:trim(binary_to_list(Binary)), "\n\r").

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    Graph = make_digraph(Input, [acyclic]),
    sum_of_orbits(Graph).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    Graph = make_digraph(Input, [cyclic]),
    orbital_dist(Graph).

-spec make_digraph(input_type(), [acyclic | cyclic]) -> digraph:graph().
make_digraph(Input, DigraphOpts) ->
    Atom = fun list_to_atom/1,
    Graph = digraph:new(DigraphOpts),
    lists:map(fun(Line) ->
                 [A, B] = lists:map(Atom, string:tokens(Line, ")")),
                 digraph:add_vertex(Graph, A),
                 digraph:add_vertex(Graph, B),
                 digraph:add_edge(Graph, B, A, {B, A}),
                 digraph:add_edge(Graph, A, B, {A, B})
              end,
              Input),
    Graph.

-spec sum_of_orbits(digraph:graph()) -> number().
sum_of_orbits(Graph) ->
    lists:sum(
        lists:map(fun ('COM') ->
                          0;
                      (V) ->
                          length(digraph:get_path(Graph, V, 'COM')) - 1
                  end,
                  digraph:vertices(Graph))).

-spec orbital_dist(digraph:graph()) -> integer().
orbital_dist(Graph) ->
    length(digraph:get_path(Graph, 'YOU', 'SAN')) - 3.
