-module(aoc2020_day25).

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-include("aoc_puzzle.hrl").

-define(NUM_20201227, 20201227).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2020,
                day = 25,
                name = "Combo Breaker",
                expected = 1890859,
                has_input_file = false,
                use_one_solver_fun = true}.

-type input_type() :: {CardPK :: integer(), DoorPK :: integer()}.
-type result1_type() :: integer().

-spec parse(_) -> input_type().
parse(_) ->
    {15113849, %% Card public key
     4206373}.  %% Door public key

-spec solve(Input :: input_type()) -> result1_type().
solve({CardPK, DoorPK}) ->
    %% The problem is symmetrical. We happen to know that the door's
    %% loop size is smaller (~1M vs ~10M), so that will be faster both
    %% in terms of finding the loop size and calculating the key.
    DLS = find_loop_size(7, DoorPK),
    powmod(CardPK, DLS, ?NUM_20201227).

%%==============================================================================
%% Helpers
%%==============================================================================

find_loop_size(Subject, PK) ->
    find_loop_size(1, Subject, Subject, PK).

find_loop_size(N, Subject, Current, PK) ->
    PK0 = Current * Subject rem ?NUM_20201227,
    if PK0 == PK ->
           N + 1;
       true ->
           find_loop_size(N + 1, Subject, PK0, PK)
    end.

powmod(A, B, M) ->
    powmod(A, B, M, 1).

powmod(_, 0, _, R) ->
    R;
powmod(A, B, M, R) when B rem 2 == 1 ->
    powmod(A, B - 1, M, A * R rem M);
powmod(A, B, M, R) ->
    powmod(A * A rem M, B div 2, M, R).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
