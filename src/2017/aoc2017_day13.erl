-module(aoc2017_day13).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 13,
                name = "Packet Scanners",
                expected = {788, 3905748},
                has_input_file = true}.

-type input_type() :: [{Depth :: integer(),
                        Range :: integer(),
                        Period :: integer()}].
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    lists:map(
      fun(Line) ->
              [Depth, Range] = string:tokens(Line, ": "),
              RangeInt = list_to_integer(Range),
              Period = ((RangeInt - 1) * 2), %% pre-compute the period
              {list_to_integer(Depth), RangeInt, Period}
      end,
      string:tokens(binary_to_list(Binary), "\n\r")).

%% For part 1, we move the packet through the firewall once and see
%% how many times we get caught.
-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    move_packet(Input).

move_packet(Input) ->
    lists:foldl(
      fun({Depth, Range, Period}, Acc) ->
              case Depth rem Period of
                  0 -> Acc + (Depth * Range);
                  _ -> Acc
              end
      end, 0, Input).


%% For part 2, we check how many picoseconds we need to delay the
%% first step before moving through the firewall without getting
%% caught once.
-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    find_delay(0, Input).

%% Check delays one at a time until we find one where we aren't
%% caught. This is probably not the most efficient solution, but good
%% enough.
-spec find_delay(integer(), input_type()) -> integer().
find_delay(Delay, Input) ->
    case move_packet2(Delay, Input) of
        true -> Delay;
        _ -> find_delay(Delay + 1, Input)
    end.

%% The important observation here is that we should return immediately
%% when we detect that we have been caught. In most cases we will be
%% caught early on.
-spec move_packet2(integer(), input_type()) -> boolean().
move_packet2(_Delay, []) ->
    true;
move_packet2(Delay, [{Depth, _, Period}|_]) when (Depth + Delay) rem Period == 0 ->
    false;
move_packet2(Delay, [_|Rest]) ->
    move_packet2(Delay, Rest).
