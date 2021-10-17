%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(aoc2020_day19).

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
             , day = 19
             , name = "Monster Messages"
             , expected = {147, 263}
             , has_input_file = true
             }.

%%==============================================================================
%% Types
%%==============================================================================
-type rules() :: map(). %% TODO refine
-type messages() :: [binary()].
-type input_type() :: { rules(), messages() }.
-type result1_type() :: integer().
-type result2_type() :: result1_type().

%%------------------------------------------------------------------------------
%% @doc parse/1
%% Parses input file.
%% @end
%%------------------------------------------------------------------------------
-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  [RuleBlock, MessageBlock] = binary:split(Input, <<"\n\n">>),
  Rules = binary:split(RuleBlock, <<"\n">>, [global]),
  Messages = binary:split(MessageBlock, <<"\n">>, [global]),
  Rules0 = lists:foldl(fun parse_rule/2, #{}, Rules),
  {Rules0, Messages}.

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(Input :: input_type()) -> result1_type().
solve1({Rules, Messages}) ->
  RE = to_regex(0, Rules),
  {ok, CompiledRE} = re:compile("^" ++ RE ++ "$"),
  lists:foldl(fun(M, Acc) ->
                  case re:run(M, CompiledRE) of
                    {match, _} -> Acc + 1;
                    _ -> Acc
                  end
              end, 0, Messages).

%% ======================================================================
%% @doc solve2/1
%%
%% Part 2
%%
%% The input rules are modified so that they contain loops. The
%% modified rules are such that
%%
%% 0 : (at least one copy of rule 42)
%%     (N copies of rule 31)
%%     (N copies of rule 42)
%%
%% Regular expressions are thwarted by having to match an equal amount
%% of rule 31 and rule 42.
%%
%% So, we compute the (finite) regexps for rule 31 and 42, then match
%% *backwards*:
%%
%% 1. As many copies of rule 31 we can find
%% 2. Match the same number of copies of rule 42
%% 3. Remaining string must consist of any number of rule 42, but
%%    nothing else.
%%
%% ======================================================================
-spec solve2(Input :: input_type()) -> result2_type().
solve2({Rules, Messages}) ->
  RE_42_str = to_regex(42, Rules),
  {ok, RE_42} = re:compile("^(" ++ RE_42_str ++ ")+$"),
  {ok, RE_42_end} = re:compile("^(?<first>.*)" ++ RE_42_str ++ "$"),
  {ok, RE_31_end} = re:compile("^(?<first>.*)" ++ to_regex(31, Rules) ++ "$"),

  lists:foldl(
    fun(M, Acc) ->
        case is_match(M, RE_42, RE_42_end, RE_31_end) of
          true -> Acc + 1;
          false -> Acc
        end
    end, 0, Messages).

%%==============================================================================
%% Internals
%%==============================================================================

is_match(M, RE_42, RE_42_end, RE_31_end) ->
  {N_31, Rest} = is_match_31(M, RE_31_end, 0),
  if N_31 == 0 -> false;
     true ->
      case is_match_42_n(Rest, RE_42_end, N_31) of
        {true, Rest0} ->
          is_match_42(Rest0, RE_42);
        _ ->
          false
      end
  end.

%% Matches greedily many copies of rule 31 at the end, and returns how
%% many we matched, and the remaining string.
is_match_31(M, RE_31, N) ->
  case re:run(M, RE_31, [{capture, all_names, list}]) of
    {match, [Leading]} ->
      is_match_31(Leading, RE_31, N + 1);
    _ ->
      {N, M}
  end.

%% Matches `N' copies of rule 42 at the end
is_match_42_n(Str, _RE_42_end, 0) ->
  {true, Str};
is_match_42_n(Str, RE_42_end, N) ->
  case re:run(Str, RE_42_end, [{capture, all_names, list}]) of
    {match, [Leading]} ->
      is_match_42_n(Leading, RE_42_end, N - 1);
    _ ->
      false
  end.

%% Matches any copies of rule 42
is_match_42(Str, RE_42) ->
  case re:run(Str, RE_42) of
    {match, _} -> true;
    _ -> false
  end.

%% ======================================================================
%% Regexp rewriting
%% ======================================================================

to_regex(Key, RuleMap) when is_integer(Key) ->
  case maps:get(Key, RuleMap) of
    [RHS] -> to_regex(RHS, RuleMap);
    [_|_] = RHS ->
      "(" ++
        lists:join(
          "|",
          lists:map(
            fun(X) ->
                to_regex(X, RuleMap)
            end,
            RHS)) ++ ")"
  end;
to_regex(Key, RuleMap) when is_list(Key) ->
  lists:join(
    "",
    lists:map(
      fun(X) ->
          to_regex(X, RuleMap)
      end,
      Key));
to_regex(Key, _RuleMap) when is_atom(Key) ->
  atom_to_list(Key).

%% ======================================================================
%% Parser
%% ======================================================================

parse_rule(Binary, Acc) ->
  [LHS, RHS] = binary:split(Binary, <<":">>),
  SubRules = lists:map(
               fun(B) ->
                   lists:filtermap(
                     fun(B0) ->
                         case byte_size(B0) > 0 of
                           true -> {true, btoi(B0)};
                           false -> false
                         end
                     end,
                     binary:split(B, <<" ">>, [global]))
               end, binary:split(RHS, <<"|">>, [global])),
  LHS0 = btoi(LHS),
  maps:put(LHS0, SubRules, Acc).

btoi(B) ->
  %% TODO compile this RE to speed up parsing
  case re:run(B, "\"([ab])\"|(\\d+)", [{capture, all_but_first, list}]) of
    {match, [[], RuleNum]} ->
      list_to_integer(RuleNum);
    {match, [Terminal]} ->
      list_to_atom(Terminal)
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
