-module(aoc2016_day20).

-define(MAX_IP, 4294967295).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 20,
                name = "Firewall Rules",
                expected = {17348574, 104},
                has_input_file = true}.

-type input_type() :: [{integer(), integer()}].
-type result1_type() :: any().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    Lines = string:tokens(binary_to_list(Input), "\n\r"),
    lists:sort(
      lists:map(fun(Line) ->
                    [L, U] = string:tokens(Line, " -"),
                    {list_to_integer(L),
                     list_to_integer(U)}
                end, Lines)).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    lowest_non_blocked_ip(Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    num_non_blocked_ips(Input).

lowest_non_blocked_ip(Ranges) ->
  lowest_non_blocked_ip(Ranges, 0).

lowest_non_blocked_ip(Ranges, N) ->
  case find_first_range(N, Ranges) of
    {_Lower, Upper} ->
      %% N is blocked because it is in the range {Lower, Upper}.
      %% Continue with the first integer *not* blocked by this range.
      lowest_non_blocked_ip(Ranges, Upper + 1);
    false ->
      %% N is not blocked.
      N
  end.

%% Return the first range which covers the given integer, or false if
%% there is no such range.
find_first_range(_N, []) -> false;
find_first_range(N, [{L, U} = Range|_]) when (N >= L) and (N =< U) ->
  Range;
find_first_range(N, [_|Rest]) ->
  find_first_range(N, Rest).

%% Return the number of IPs which are not blocked.
num_non_blocked_ips(Ranges) ->
  num_non_blocked_ips(Ranges, 0, 0).

num_non_blocked_ips(Ranges, Acc, N) when N =< ?MAX_IP ->
  %% Find the next non-blocked IP.
  case lowest_non_blocked_ip(Ranges, N) of
    NB when NB =< ?MAX_IP ->
      %% Next non-blocked IP is a valid IP, so increment acc and
      %% continue with next.
      num_non_blocked_ips(Ranges, Acc + 1, NB + 1);
    _ ->
      %% Next non-blocked IP is not a valid IP, so we're done.
      Acc
  end.
