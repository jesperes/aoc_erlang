%%% Advent of Code solution for 2019 day 17.
%%% Created: 2019-12-17T16:31:23+00:00

-module(aoc2019_day17).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 17,
                name = "Set and Forget",
                expected = {4800, 982279},
                has_input_file = true}.

-type input_type() :: intcode:intcode_program().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    intcode:parse(Binary).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Prog) ->
    {_, List} = intcode:execute(Prog),
    L = lists:reverse(List),
    Bin = list_to_binary(L),
    [{Width, _}|_] = binary:matches(Bin, <<"\n">>),
    Scaffolds = make_grid(Bin, Width + 1),
    count_intersections(Scaffolds).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Prog) ->
    %% Split the necessary instructions into functions < 20 chars/line.
    %% This part was done by hand.
    A = "L,12,L,12,L,6,L,6",
    B = "R,8,R,4,L,12",
    C = "L,12,L,6,R,12,R,8",
    M = "A,B,A,C,B,A,C,B,A,C",

    Input = io_lib:format("~s~n~s~n~s~n~s~nn~n", [M, A, B, C]),
    Prog0 = maps:merge(Prog, #{0 => 2}),
    %% Score is the last integer output by the program, but this
    %% implementation returns the output in reverse order.
    {_, [Score|_]} = intcode:execute(Prog0, Input),
    Score.

count_intersections(Scaffolds) ->
  maps:fold(
    fun({X, Y}, _, Acc) ->
        N = {X, Y - 1},
        E = {X + 1, Y},
        S = {X, Y + 1},
        W = {X - 1, Y},
        case maps:is_key(N, Scaffolds) andalso
          maps:is_key(S, Scaffolds) andalso
          maps:is_key(E, Scaffolds) andalso
          maps:is_key(W, Scaffolds) of
          true ->
            Acc + (X * Y);
          false ->
            Acc
        end
    end, 0, Scaffolds).

make_grid(Bin, Width) ->
  ScaffoldingChars = [<<"#">>, <<"<">>, <<">">>, <<"v">>, <<"^">>],
  maps:from_list(
    [{{Start rem Width, Start div Width}, $*}
     || {Start, _} <- binary:matches(Bin, ScaffoldingChars)]).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
