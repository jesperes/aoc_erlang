-module(aoc_template1).

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 0,
                day = 0,
                name = "TBD",
                expected = {0, 0},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: any().
-type result_type() :: {integer(), integer()}.

-spec parse(Binary :: binary()) -> input_type().
parse(_Binary) ->
    ok.

-spec solve(Input :: input_type()) -> result_type().
solve(_Input) ->
    {0, 0}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
