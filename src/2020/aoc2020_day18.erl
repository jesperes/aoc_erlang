%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(aoc2020_day18).
-behavior(aoc_puzzle).

-export([ parse/1
        , solve1/1
        , solve2/1
        , info/0
        ]).

-include("aoc_puzzle.hrl").

%%------------------------------------------------------------------------------
%% @doc info/0
%% Returns info about this puzzle.
%% @end
%%------------------------------------------------------------------------------
-spec info() -> aoc_puzzle().
info() ->
  #aoc_puzzle{ module = ?MODULE
             , year = 2020
             , day = 18
             , name = "Operation Order"
             , expected = {8298263963837, 145575710203332}
             , has_input_file = true
             }.

%%==============================================================================
%% Types
%%==============================================================================
-type input_type() :: [string()].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

%%------------------------------------------------------------------------------
%% @doc parse/1
%% Parses input file.
%% @end
%%------------------------------------------------------------------------------
-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  string:tokens(binary_to_list(Input), "\r\n").

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(Input :: input_type()) -> result1_type().
solve1(Lines) ->
  sum_over_lines(Lines, aoc2020_day18_part1_parser).

%%------------------------------------------------------------------------------
%% @doc solve2/1
%% Solves part 2. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve2(Input :: input_type()) -> result2_type().
solve2(Lines) ->
  sum_over_lines(Lines, aoc2020_day18_part2_parser).

%%==============================================================================
%% Internals
%%==============================================================================

sum_over_lines(Lines, ParserModule) ->
  lists:foldl(
    fun(Line, Sum) ->
        {ok, Tokens, _} = aoc2020_day18_lexer:string(Line),
        {ok, AST} = ParserModule:parse(Tokens),
        Sum + eval_ast(AST)
    end, 0, Lines).

eval_ast({int, N}) -> N;
eval_ast({expr, Expr}) -> eval_ast(Expr);
eval_ast({plus, A, B}) -> eval_ast(A) + eval_ast(B);
eval_ast({mult, A, B}) -> eval_ast(A) * eval_ast(B).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
