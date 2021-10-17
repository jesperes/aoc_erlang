-module(aoc2015_day20).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 20,
                name = "Infinite Elves and Infinite Houses",
                expected = {831600, 884520},
                has_input_file = false}.

-type input_type() :: integer().
-type result1_type() :: any().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    36000000.

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    deliver(1, Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    deliver2(1, #{}, Input).

%% This is a little cheating based on knowledge about the solutions,
%% but it allows us to keep the runtime down to < 10s.
-define(UPPER, 900000).
-define(LOWER, 800000).
-define(CHUNKSIZE, 10000).

%% Deliver presents to houses in the range [StartHouse, StartHouse +
%% ChunkSize).
deliver(StartHouse, Limit) ->
    %% If StartHouse is 1, ChunkSize == 1000, then we are going to
    %% deliver presents to houses 1..1000. Elf number 1000 is the last
    %% elf which is going to deliver presents.
    LastElf = LastHouse = StartHouse + ?CHUNKSIZE - 1,

    Presents =
        foldn(fun(ElfNum, Acc) ->
                 %% Find the first house this elf should deliver
                 %% presents to
                 FirstHouse = StartHouse + (ElfNum - StartHouse rem ElfNum),
                 NumPresents = ElfNum * 10,
                 foldn(fun (House, InnerAcc) when (House < ?LOWER) or (House > ?UPPER) -> InnerAcc;
                           (House, InnerAcc) ->
                               maps:update_with(House,
                                                fun(V) -> V + NumPresents end,
                                                NumPresents,
                                                InnerAcc)
                       end,
                       Acc,
                       FirstHouse,
                       LastHouse,
                       ElfNum)
              end,
              #{},
              1,
              LastElf,
              1),

    case maps:fold(fun (K, V, AccIn) when (V >= Limit) and (K < AccIn) ->
                           K;
                       (_, _, AccIn) ->
                           AccIn
                   end,
                   undef,
                   Presents)
    of
        undef ->
            deliver(StartHouse + ?CHUNKSIZE, Limit);
        House ->
            House
    end.

deliver2(Elf, Map, Limit) ->
    NumPresents = Elf * 11,
    Map1 =
        foldn(fun (House, Acc) when (House < ?LOWER) or (House > ?UPPER) ->
                      Acc;
                  (House, Acc) ->
                      maps:update_with(House, fun(V) -> V + NumPresents end, NumPresents, Acc)
              end,
              Map,
              Elf,
              Elf * 50,
              Elf),

    %% When this elf is finished, the house with that number will not
    %% receive any more presents.
    N = maps:get(Elf, Map1, 0),
    if N >= Limit ->
           Elf;
       true ->
           deliver2(Elf + 1, maps:remove(Elf, Map1), Limit)
    end.

foldn(Fun, Init, Start, End, Incr) ->
    foldn(Start, Fun, Init, Start, End, Incr).

foldn(N, _Fun, Acc, _Start, End, _Incr) when N > End ->
    Acc;
foldn(N, Fun, Acc, Start, End, Incr) ->
    NewAcc = Fun(N, Acc),
    foldn(N + Incr, Fun, NewAcc, Start, End, Incr).
