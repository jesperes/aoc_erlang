-module(aoc2018_day04).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2018,
                day = 4,
                name = "Repose Record",
                expected = {140932, 51232},
                has_input_file = true}.

-type log_entry() ::
    {Year :: integer(),
     Month :: integer(),
     Day :: integer(),
     GuardNum :: string(),
     StartMin :: integer(),
     EndMin :: integer()}.
-type input_type() :: [log_entry()].
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    scan_input(lists:map(fun parse_date_str/1,
                         lists:sort(
                             string:tokens(binary_to_list(Input), "\n\r")))).

-spec solve1(Input :: input_type()) -> result1_type().
solve1(LogEntries) ->
    %% Construct a list of ranges for each sleeping period
    SleepRanges = count_sleep_minutes(LogEntries),

    %% Given this list of sleep ranges, find the sleepiest guard.
    SleepyGuard = find_sleepiest_guard(SleepRanges),

    %% Given the list of log entries and the sleepiest guard, find the
    %% minute where the guard sleeps the most, summing over all days
    %% the guard sleeps.
    SleepiestMinute = find_sleepiest_minute(SleepyGuard, LogEntries),

    [$# | GuardNumStr] = SleepyGuard,
    GuardNum = list_to_integer(GuardNumStr),
    GuardNum * SleepiestMinute.

-spec solve2(Input :: input_type()) -> result2_type().
solve2(LogEntries) ->
    %% Here we use a different strategy to find the sleepiest guard.
    %% Over all the guards, which guard is most frequently asleep on
    %% the same minute.
    %% This map maps {Guard,Minute} tuples to the total number of
    %% minutes that guard sleeps any given minute.
    Map = lists:foldl(fun(Min, MapAcc) -> guards_asleep_at(LogEntries, Min, MapAcc) end,
                      #{},
                      lists:seq(0, 59)),

    %% Find the larges value in the map
    {GuardId, SleepiestMinute, _NumSleeps} =
        maps:fold(fun ({Guard, Minute} = _Key, NumSleeps = _Value, {_CurrGuard, _CurrMinute, Max})
                          when NumSleeps > Max ->
                          {Guard, Minute, NumSleeps}; %% we have a new max, at 'Minute'
                      (_, _, AccIn) ->
                          AccIn
                  end,
                  {undefined, undefined, 0},
                  Map),

    [$# | GuardNum] = atom_to_list(GuardId),
    list_to_integer(GuardNum) * SleepiestMinute.

parse_date_str(Str) ->
    string:tokens(Str, "[-] :").

int(N) ->
    list_to_integer(N).

-spec scan_input([[string()]]) -> input_type().
    scan_input(LogEntries) ->
    %% tuple is { GuardNum, FallAsleepTime }
    scan_input(LogEntries, {noguard, notime}).

-spec scan_input([[string()]], {any(), any()}) -> input_type().
scan_input([], _) ->
    [];
scan_input([[_, _, _, _, _, "Guard", GuardNum, "begins", "shift"] | Rest], {_, _}) ->
    scan_input(Rest, {GuardNum, onduty});
scan_input([[_, _, _, _, M, "falls", "asleep"] | Rest], {GuardNum, _}) ->
    scan_input(Rest, {GuardNum, M});
scan_input([[Y, M, D, _H, EndMin, "wakes", "up"] | Rest], {GuardNum, StartMin}) ->
    [{int(Y), int(M), int(D), GuardNum, int(StartMin), int(EndMin)} | scan_input(Rest,
                                                                                 {GuardNum,
                                                                                  onduty})].

%% Count the number of minutes slept by each guard
count_sleep_minutes(List) ->
    count_sleep_minutes(List, #{}).

count_sleep_minutes([], Map) ->
    Map;
count_sleep_minutes([{_Y, _M, _D, Guard, Start, End} | Rest], Map) ->
    Duration = End - Start,
    NewMap = maps:update_with(Guard, fun(V) -> V + Duration end, Duration, Map),
    count_sleep_minutes(Rest, NewMap).

%% Find the guard which has slept the most number of minutes
%% (i.e. find the key with the largest value in the given map)
find_sleepiest_guard(Map) ->
    {Guard, _} =
        maps:fold(fun (K, V, {_, CurrMax}) when V >= CurrMax ->
                          {K, V};
                      (_K, _V, AccIn) ->
                          AccIn
                  end,
                  {undefined, 0},
                  Map),
    Guard.

find_sleepiest_minute(Guard, Intervals) ->
    GuardList = lists:filter(fun({_, _, _, Guard0, _, _}) -> Guard0 =:= Guard end, Intervals),

    %% GuardList contains dates and intervals for when the given guard
    %% was sleeping.
    %%
    %% [{1518,11,1,"#10",5,25},
    %%  {1518,11,1,"#10",30,55},
    %%  {1518,11,3,"#10",24,29}]
    NumSleepsAtMinuteList =
        lists:map(fun(Minute) -> {Minute, number_of_sleeping_days(GuardList, Minute)} end,
                  [Min || Min <- lists:seq(0, 59)]),

    find_max_sleeping_days(NumSleepsAtMinuteList, 0, notfound).

%% Returns the number of days a guard has been sleeping at the given
%% minute.
number_of_sleeping_days([], _) ->
    0;
number_of_sleeping_days([{_Y, _M, _D, _Guard, Start, End} | Rest], Minute) ->
    if (Minute >= Start) and (Minute < End) ->
           1 + number_of_sleeping_days(Rest, Minute);
       true ->
           number_of_sleeping_days(Rest, Minute)
    end.

find_max_sleeping_days([], _Max, Minute) ->
    Minute;
find_max_sleeping_days([{Minute, NumSleepsAtMinute} | Rest], Max, _)
    when NumSleepsAtMinute > Max ->
    find_max_sleeping_days(Rest, NumSleepsAtMinute, Minute);
find_max_sleeping_days([_ | Rest], Max, Minute) ->
    find_max_sleeping_days(Rest, Max, Minute).

%% Update a map with which guards are asleep at which minute
guards_asleep_at(SleepRanges, Minute, Map) ->
    lists:foldl(fun({_Y, _M, _D, Guard, Start, End}, MapAcc) ->
                   if (Minute >= Start) and (Minute < End) ->
                          maps:update_with({list_to_atom(Guard), Minute},
                                           fun(V) -> V + 1 end,
                                           1,
                                           MapAcc);
                      true -> MapAcc
                   end
                end,
                Map,
                SleepRanges).
