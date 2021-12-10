-module(aoc2021_day10).

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 10,
                name = "Syntax Scoring",
                expected = {392043, 1605968119},
                has_input_file = true,
                use_one_solver_fun = true}.

-type input_type() :: [binary()].
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    binary:split(Binary, <<"\n">>, [trim_all, global]).

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    {P1, P2} =
        lists:foldl(fun(Line, {P1, P2}) ->
                       M = match_pairs(Line),
                       case M of
                           {nomatch, Rest} -> {count(Rest) + P1, P2};
                           {incomplete, Completion} -> {P1, [completion_score(Completion, 0) | P2]}
                       end
                    end,
                    {0, []},
                    Input),
    {P1, middle_score(P2)}.

middle_score(List) ->
    L0 = lists:sort(List),
    Len = length(L0),
    lists:nth(Len div 2 + 1, L0).

count({nomatch, Bin}) ->
    count(Bin);
count(<<$), _/binary>>) ->
    3;
count(<<$], _/binary>>) ->
    57;
count(<<$}, _/binary>>) ->
    1197;
count(<<$>, _/binary>>) ->
    25137;
count(_) ->
    0.

completion_score(<<>>, Acc) ->
    Acc;
completion_score(<<C, Rest/binary>>, Acc) ->
    completion_score(Rest, Acc * 5 + score(C)).

score($)) ->
    1;
score($]) ->
    2;
score($}) ->
    3;
score($>) ->
    4.

-define(IS_PAIR1(A, B), A =:= $( andalso B =:= $)).
-define(IS_PAIR2(A, B), A =:= $[ andalso B =:= $]).
-define(IS_PAIR3(A, B), A =:= ${ andalso B =:= $}).
-define(IS_PAIR4(A, B), A =:= $< andalso B =:= $>).
-define(IS_PAIR(A, B),
        ?IS_PAIR1(A, B) orelse ?IS_PAIR2(A, B) orelse ?IS_PAIR3(A, B) orelse ?IS_PAIR4(A, B)).
-define(IS_OPEN(A), A =:= $( orelse A =:= $[ orelse A =:= ${ orelse A =:= $<).
-define(IS_CLOSE(A), A =:= $) orelse A =:= $] orelse A =:= $} orelse A =:= $>).

match_pairs(<<>>) ->
    <<>>;
match_pairs(<<Opening, Closing, Rest/binary>> = _Binary)
    when ?IS_PAIR(Opening, Closing) ->
    match_pairs(Rest);
match_pairs(<<Closing, _/binary>> = Binary) when ?IS_CLOSE(Closing) ->
    Binary;
match_pairs(<<Opening, Rest/binary>> = _Binary) when ?IS_OPEN(Opening) ->
    case match_pairs(Rest) of
        <<Closing, Rest0/binary>> when ?IS_PAIR(Opening, Closing) ->
            match_pairs(Rest0);
        <<>> ->
            {incomplete, <<(other(Opening))>>};
        {incomplete, Bin} ->
            {incomplete, <<Bin/binary, (other(Opening))>>};
        {nomatch, _} = NoMatch ->
            NoMatch;
        NoMatch ->
            {nomatch, NoMatch}
    end.

other($() ->
    $);
other($[) ->
    $];
other(${) ->
    $};
other($<) ->
    $>.

%% Tests

-ifdef(TEST).

match_pairs_test() ->
    <<>> = match_pairs(<<"()">>),
    <<>> = match_pairs(<<"()()">>),
    <<"))">> = match_pairs(<<"()()))">>),
    <<>> = match_pairs(<<"(())">>),
    {nomatch, <<"})">>} = match_pairs(<<"(()[]})">>),
    <<>> = match_pairs(<<"{()()()}">>),
    <<>> = match_pairs(<<"<([{}])>">>),
    <<>> = match_pairs(<<"[<>({}){}[([])<>]]">>),
    {nomatch, <<")">>} = match_pairs(<<"<([]){()}[{}])">>),
    {incomplete, <<"}}">>} = match_pairs(<<"{{">>).

ex1_test() ->
    Binary =
        <<"[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{"
          "[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}"
          "([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([("
          "[[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]">>,
    Input = parse(Binary),
    {26397, _} = solve(Input).

ex2_test() ->
    {nomatch, <<"}>">>} = match_pairs(<<"{[<>[]}>">>),
    {nomatch, <<"}", _/binary>>} = match_pairs(<<"{([(<{}[<>[]}>{[]{[(<()>">>).

ex3_test() ->
    {incomplete, <<"}])">>} = match_pairs(<<"([{">>).

completion_score_test() ->
    288957 = completion_score(<<"}}]])})]">>, 0).

middle_score_test() ->
    4 = middle_score([4, 3, 2, 5, 6]).

-endif.
