-module(aoc2018_day21).

%%
%% We are given a program (input.txt) in ElfCode (see days 16
%% and 19), and we are to determine certain properties of it. I
%% started out by "emulating" it only to discover that it was far to
%% inefficient to get anything done. I then translated the program
%% more or less directly to C and got the solution that
%% way.
%%
%% The code below (outer_loop/3) is roughly this C program translated to erlang.
%%

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2018,
                day = 21,
                name = "Chronal Conversion",
                expected = {4682012, 5363733},
                has_input_file = false}.

-type input_type() :: none.
-type result_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    %% The program was the input, and it has been hand-converted into
    %% Erlang.
    none.

-spec solve1(Input :: input_type()) -> result_type().
solve1(_) ->
    %% For part 1, we are only interested in finding out the value of
    %% R0 which causes the program to terminate in as few steps as
    %% possible. This means finding the R1 value after running through
    %% the program once (there is an "outer" outer loop in the
    %% original program which is basically "do ... while (R1 != R0)".)
    %%
    %% So, for part 1, we simply need to know the value of R1 after
    %% one pass through outer_loop. If R0 is set to this value, the
    %% program will terminate after as few steps as possible.
    {R1, _, _} = outer_loop(0),
    R1.

-spec solve2(Input :: input_type()) -> result_type().
solve2(_) ->
    %% For part 2, we are told that we want to figure out the smallest
    %% value of R0 which causes the program to terminate in as many
    %% steps as possible. I did not understand this part until a
    %% colleague explained it to me, but basically we can observe that
    %% as we set R0 to {1, 2, 3, ...}, the resulting R1 values will
    %% eventually repeat, and we want to know the last of the R0 values
    %% used before we observe a repetition in the R1 values. This is the
    %% (smallest) R0 value which causes the program to run in as many
    %% cycles as possible.
    %%
    %% We solve this by putting all the R1 values in a set, and
    %% terminate once we get a R1 value we have already seen.
    part2_loop(0, sets:new()).

part2_loop(R1, Vals) ->
  {R1_new, _, _} = outer_loop(R1),
  case sets:is_element(R1_new, Vals) of
    true ->
      R1;
    false ->
      part2_loop(R1_new, sets:add_element(R1_new, Vals))
  end.

%% The outer loop runs a single pass through the computation,
%% producing new values for the registers R1, R2, R3, and R5.  The
%% code doesn't really do anything useful.
%%
%% In the original program, there is an additional loop outside this
%% one which terminates once R1 == R0.
outer_loop(R1) ->
  inner_loop1(8725355, R1 bor 65536).

inner_loop1(R1, R2) ->
  R5 = R2 band 255,
  R1_1 = ((R1 + R5) * 65899) band 16#ffffff,
  inner_loop2(R1_1, R2, R5).
inner_loop2(R1, R2, _) when R2 >= 256 ->
  R5 = inner_loop3(R2, 0),
  inner_loop1(R1, R5);
inner_loop2(R1, R2, R5) ->
  {R1, R2, R5}.

%% Computes a new value for R5. The program spends 99% of all its time
%% here.
inner_loop3(R2, R5) ->
  if (R5 + 1) bsl 8 > R2 ->
      R5;
     true ->
      inner_loop3(R2, R5 + 1)
  end.
