-module(aoc2015_day04).

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 4,
                name = "The Ideal Stocking Stuffer",
                expected = {282749, 9962624},
                use_one_solver_fun = true,
                has_input_file = false}.

-type input_type() :: string().
-type result_type() :: {integer(), integer()}.

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    "yzbqklnj".

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    solve(Input, 0, undef).

solve(Input, N, P1) ->
    case erlang:md5(Input ++ integer_to_list(N)) of
        <<0, 0, 0, _/binary>> ->
            %% Found part 2, we are done
            {P1, N};
        <<0, 0, 0:4, _/bitstring>> when P1 =:= undef ->
            %% Found part 1 solution, continue with part 2.
            solve(Input, N + 1, N);
        _ ->
            solve(Input, N + 1, P1)
    end.
