-module(aoc2017_day16).

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 16,
                name = "Permutation Promenade",
                expected = {<<"kbednhopmfcjilag">>, <<"fbmcgdnjakpioelh">>},
                use_one_solver_fun = true,
                has_input_file = true}.

-type state() :: binary().
-type input_type() ::
    [{s, X :: integer()} |
     {x, A :: integer(), B :: integer()} |
     {p, A :: integer(), B :: integer()}].
-type result_type() :: {state(), state()}.

-define(INIT_STATE, <<"abcdefghijklmnop">>).

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    lists:map(fun(Elem) ->
                 [Move | Rest] = string:trim(Elem),
                 case Move of
                     $s -> {s, list_to_integer(Rest)};
                     $p ->
                         [[A], [B]] = string:tokens(Rest, "/"),
                         {p, A, B};
                     $x ->
                         [A, B] = string:tokens(Rest, "/"),
                         {x, list_to_integer(A), list_to_integer(B)}
                 end
              end,
              string:tokens(binary_to_list(Binary), ",")).

%% Find the cycle length. Since all the operations are reversible,
%% the cycle needs to start at position 0. When we have found the
%% cycle length, we can easily pick out the answers.
-spec solve(input_type()) -> result_type().
solve(Moves) ->
    NumRounds = 1000000000,
    {CycleLen, States} = find_cycle(?INIT_STATE, Moves, sets:new(), #{}, 0),
    Part1 = maps:get(1, States),
    Part2 = maps:get(NumRounds rem CycleLen, States),
    {Part1, Part2}.

do_move(Move, State) ->
    case Move of
        {s, X} ->
            spin(State, X);
        {x, A, B} ->
            exchange(State, A, B);
        {p, A, B} ->
            partner(State, A, B)
    end.

dance(Start, Moves) ->
    lists:foldl(fun do_move/2, Start, Moves).

find_cycle(CurrState, Moves, Cache, States, Current) ->
    States0 = maps:put(Current, CurrState, States),
    case sets:is_element(CurrState, Cache) of
        false ->
            NextState = dance(CurrState, Moves),
            find_cycle(NextState, Moves, sets:add_element(CurrState, Cache), States0, Current + 1);
        true ->
            {Current, States0}
    end.

%% Dance moves
spin(Binary, X) ->
    {A, B} = split_binary(Binary, byte_size(Binary) - X),
    <<B/binary, A/binary>>.

exchange(Binary, A, B) ->
    XA = binary:at(Binary, A),
    XB = binary:at(Binary, B),
    S0 = binary:replace(Binary, <<XA>>, <<XB>>, [{scope, {A, 1}}]),
    S1 = binary:replace(S0, <<XB>>, <<XA>>, [{scope, {B, 1}}]),
    S1.

partner(Binary, A, B) ->
    {APos, _} = binary:match(Binary, <<A>>),
    {BPos, _} = binary:match(Binary, <<B>>),
    exchange(Binary, APos, BPos).
