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
                expected = {0, 0},
                has_input_file = true}.

-type input_type() :: [{[binary()], [binary()]}].
-type result_type() :: integer().

%% 1 -> 2 segments
%% 4 -> 4 segments
%% 7 -> 3 segments
%% 8 -> 7 segments
%%
%% 2 -> 5 segments
%% 3 -> 5 segments
%% 5 -> 5 segments
%%
%% 0 -> 6 segments
%% 6 -> 6 segments
%% 9 -> 6 segments

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    lists:map(fun(B) ->
                 [L, R] = binary:split(B, <<"|">>),
                 {binary:split(L, <<" ">>, [trim_all, global]),
                  binary:split(R, <<" ">>, [trim_all, global])}
              end,
              binary:split(Binary, <<"\n">>, [trim_all, global])).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Lines) ->
    lists:foldl(fun(E, Acc) ->
                   case size(E) of
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
    deduce(L).

deduce(L) ->
    Map = #{$a => <<"abcdefg">>,
            $b => <<"abcdefg">>,
            $c => <<"abcdefg">>,
            $d => <<"abcdefg">>,
            $e => <<"abcdefg">>,
            $f => <<"abcdefg">>,
            $g => <<"abcdefg">>,
            lists:foldl(fun(Pattern, Acc) ->
                                case Pattern of
            <<X, Y>> ->
                %% Two segment display, {X, Y} is either {c, f} or {f, c}
                maps:merge(Acc,
                    #{X => <<"cf">>,
                      Y => <<"cf">>}
                    )
        end
    end, Map, L).

%% Tests
-ifdef(TEST).

%% ...

-endif.
