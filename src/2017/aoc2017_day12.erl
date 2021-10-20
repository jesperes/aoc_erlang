-module(aoc2017_day12).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 12,
                name = "Digital Plumber",
                expected = {141, 171},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: [{integer(), [integer()]}].
-type result_type() :: {integer(), integer()}.

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    lists:map(fun(Line) ->
                 string:tokens(Line, " <>-,")
              end,
              string:tokens(binary_to_list(Binary), "\n\r")).

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    G = digraph:new([acyclic]),
    lists:foreach(fun([Src|Dest]) ->
                     VSrc = digraph:add_vertex(G, Src),
                     lists:foreach(fun(D) ->
                                      VDest = digraph:add_vertex(G, D),
                                      digraph:add_edge(G, VSrc, VDest)
                                   end,
                                   Dest)
                  end,
                  Input),

    Components = digraph_utils:components(G),
    {value, Component0} =
        lists:search(fun(Component) -> lists:member("0", Component) end, Components),
    Part1 = length(Component0),
    Part2 = length(Components),
    {Part1, Part2}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
