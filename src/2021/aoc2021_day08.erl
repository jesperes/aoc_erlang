-module(aoc2021_day08).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 8,
                name = "Seven Segment Stretch",
                expected = {548, 1074888},
                has_input_file = true}.

-type input_type() :: [{[atom()], [atom()]}].
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    lists:map(fun(B) ->
                 [L, R] = binary:split(B, <<"|">>),
                 {split_binlist_to_atomlist(L, <<" ">>), split_binlist_to_atomlist(R, <<" ">>)}
              end,
              binary:split(Binary, <<"\n">>, [trim_all, global])).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Lines) ->
    lists:foldl(fun(E, Acc) ->
                   case length(E) of
                       2 -> Acc + 1;
                       3 -> Acc + 1;
                       4 -> Acc + 1;
                       7 -> Acc + 1;
                       _ -> Acc
                   end
                end,
                0,
                lists:foldl(fun({_, R}, Acc) -> R ++ Acc end, [], Lines)).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Lines) ->
    lists:foldl(fun({L, R}, Acc) -> output_value(L, R) + Acc end, 0, Lines).

output_value(L, R) ->
    %% Possible mappings
    M = maps:map(fun(_K, [V]) -> V end, deduce(L)),
    display_to_number(M, R).

display_to_number(M, Digits) ->
    list_to_integer(lists:map(fun(Disp) ->
                                 display_to_digit(lists:sort(
                                                      lists:map(fun(Digit) -> maps:get(Digit, M)
                                                                end,
                                                                Disp)))
                              end,
                              Digits)).

display_to_digit([a, b, c, e, f, g]) ->
    $0;
display_to_digit([c, f]) ->
    $1;
display_to_digit([a, c, d, e, g]) ->
    $2;
display_to_digit([a, c, d, f, g]) ->
    $3;
display_to_digit([b, c, d, f]) ->
    $4;
display_to_digit([a, b, d, f, g]) ->
    $5;
display_to_digit([a, b, d, e, f, g]) ->
    $6;
display_to_digit([a, c, f]) ->
    $7;
display_to_digit([a, b, c, d, e, f, g]) ->
    $8;
display_to_digit([a, b, c, d, f, g]) ->
    $9.

deduce(L) ->
    Map = char_mapping(),
    All = [a, b, c, d, e, f, g],
    AllButCDE = All -- [c, d, e],
    lists:foldl(fun(Pattern, Acc) ->
                   NewAcc =
                       case length(Pattern) of
                           2 -> prune(Pattern, [c, f], All, Acc);
                           3 -> prune(Pattern, [a, c, f], All, Acc);
                           4 -> prune(Pattern, [b, c, d, f], All, Acc);
                           6 -> remove(All -- Pattern, AllButCDE, Acc);
                           _ -> Acc
                       end,
                   prune_fixed(NewAcc)
                end,
                Map,
                L).

%% For all known mappings, e.g. #{a => [b]}, remove 'b' from all other mappings,
%% since we know that no other letter can map to b.
prune_fixed(Map) ->
    KnownMap = maps:filter(fun(_K, V) -> length(V) == 1 end, Map),
    KnownValues =
        lists:usort(
            lists:flatten(
                maps:values(KnownMap))),

    maps:map(fun(K, V) ->
                case maps:is_key(K, KnownMap) of
                    true -> V;
                    false -> V -- KnownValues
                end
             end,
             Map).

%% Prune: when letters in Pattern are known to map to Dest, remove all
%% other mappings involving Pattern or Dest (since Pattern <=> Dest).
prune(Pattern, Dest, All, Map) ->
    Map0 = remove(Pattern, All -- Dest, Map),
    Map1 = remove(All -- Pattern, Dest, Map0),
    Map1.

%% Remove List from the possible mappings of the chars in From
remove(From, List, Acc) ->
    lists:foldl(fun(X, InnerAcc) -> maps:update_with(X, fun(Old) -> Old -- List end, InnerAcc)
                end,
                Acc,
                From).

split_binlist_to_atomlist(Binary, Sep) ->
    binlist_to_atomlist(binary:split(Binary, Sep, [trim_all, global])).

binlist_to_atomlist(List) ->
    lists:map(fun(Bin) ->
                 lists:map(fun(Char) -> list_to_atom([Char]) end, binary_to_list(Bin))
              end,
              List).

char_mapping() ->
    lists:foldl(fun(N, Acc) -> maps:put(N, [a, b, c, d, e, f, g], Acc) end,
                #{},
                [a, b, c, d, e, f, g]).

%% Tests
-ifdef(TEST).

remove_test() ->
    ?assertEqual("abefg", "abcdefg" -- "cd"),
    CharMapping = char_mapping(),
    CharMapping0 = remove([a, b], [d, c], CharMapping),
    ?assertEqual([a, b, e, f, g], maps:get(a, CharMapping0)),
    ?assertEqual([a, b, e, f, g], maps:get(b, CharMapping0)).

prune_fixes_test() ->
    ?assertEqual(#{a => [b], c => [d]}, prune_fixed(#{a => [b], c => [b, d]})).

ex1_test() ->
    Binary =
        <<"acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab "
          "| cdfeb fcadb cdfeb cdbaf">>,
    ?assertEqual(5353, solve2(parse(Binary))).

ex2_test() ->
    Binary =
        <<"be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb "
          "| fdgacbe cefdb cefbgd gcbe\n    edbfga begcd cbg gc gcadebf "
          "fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n   "
          " fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef "
          "| cg cg fdcagb cbg\n    fbegcd cbd adcefb dageb afcb bc aefdc "
          "ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n    aecbfdg "
          "fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf "
          "bgf bfgea\n    fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg "
          "bafgc acf | gebdcfa ecba ca fadegcb\n    dbcfg fgd bdegcaf "
          "fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge "
          "gbcadfe\n    bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg "
          "gebcd | ed bcgafe cdgba cbgef\n    egadfb cdbfeg cegd fecab "
          "cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n   "
          " gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc "
          "| fgae cfgab fg bagce">>,
    ?assertEqual(61229, solve2(parse(Binary))).

%% ...
-endif.
