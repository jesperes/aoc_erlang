-module(aoc2015_day01).

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 1,
                name = "Not Quite Lisp",
                expected = {232, 1783},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: binary().
-type result_type() :: {integer(), integer()}.

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    Input.

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    count_floors(Input, 0, 0, undef).

count_floors(<<>>, Acc, _, Neg) ->
    {Acc, Neg};
count_floors(Bin, Acc, Pos, undef) when Acc < 0 ->
    count_floors(Bin, Acc, Pos, Pos);
count_floors(<<$(, Rest/binary>>, Acc, Pos, Neg) ->
    count_floors(Rest, Acc + 1, Pos + 1, Neg);
count_floors(<<$), Rest/binary>>, Acc, Pos, Neg) ->
    count_floors(Rest, Acc - 1, Pos + 1, Neg).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
