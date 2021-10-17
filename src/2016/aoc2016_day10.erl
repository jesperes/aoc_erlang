-module(aoc2016_day10).

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2016,
                day = 10,
                name = "Balance Bots",
                expected = {161, 133163},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: [tuple()].
-type result_type() :: {integer(), integer()}.

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    lists:map(fun parse_line/1, string:tokens(binary_to_list(Input), "\n\r")).

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    State = run_passes(Input),
    maps:fold(fun ({bot, Bot}, V, {_, Part2} = Acc) ->
                      %% Part 1: find the bot containing chips 17 and 61.
                      case lists:sort(V) of
                          [17, 61] ->
                              {Bot, Part2};
                          _ ->
                              Acc
                      end;
                  ({output, N}, [V], {Part1, Part2}) when (N >= 0) and (N =< 2) ->
                      %% Part 2: multiply all values in the output bins {0, 1, 2}.
                      {Part1, Part2 * V};
                  (_, _, Acc) ->
                      Acc
              end,
              {0, 1},
              State).

%% Process instructions. Returns a tuple {RemainingInstructions, State}.
-spec process(Instrs :: list(), InstrAcc :: list(), State :: map()) -> {list(), map()}.
process([], InstrAcc, State) ->
    {lists:reverse(InstrAcc), State};
process([{goes_to, Value, Bot} | Rest], InstrAcc, State) ->
    %% Put value into bot, consume instruction.
    process(Rest, InstrAcc, send_to(Value, bot, Bot, State));
process([{bot_to_bot, Bot, LowType, LowDest, HighType, HighDest} = Instr | Rest],
        InstrAcc,
        State) ->
    case maps:get({bot, Bot}, State, []) of
        Values when length(Values) < 2 ->
            %% Bot has nothing to do yet. Add instruction to accumulator so
            %% it is kept the next round.
            process(Rest, [Instr | InstrAcc], State);
        Values when length(Values) == 2 ->
            %% Bot has two chips, pass them on. Consume the instruction.
            [Low, High] = lists:sort(Values),
            State1 = send_to(Low, LowType, LowDest, State),
            State2 = send_to(High, HighType, HighDest, State1),
            process(Rest, InstrAcc, State2)
    end.

send_to(Value, Type, Dest, State) ->
    maps:update_with({Type, Dest}, fun(Old) -> [Value | Old] end, [Value], State).

run_passes(Instrs) ->
    run_passes(Instrs, #{}).

%% Process the list of instructions until all instructions have been
%% processed.
run_passes([], Map) ->
    Map;
run_passes(Instrs, Map0) ->
    {InstrsOut, Map1} = process(Instrs, [], Map0),
    run_passes(InstrsOut, Map1).

parse_line(Line) ->
    [First | Rest] = string:lexemes(Line, " "),
    case First of
        "value" ->
            [Value, "goes", "to", "bot", Bot] = Rest,
            {goes_to, list_to_integer(Value), list_to_integer(Bot)};
        "bot" ->
            [Bot, "gives", "low", "to", LowType, LowDest, "and", "high", "to", HighType, HighDest] =
                Rest,
            {bot_to_bot,
             list_to_integer(Bot),
             list_to_atom(LowType),
             list_to_integer(LowDest),
             list_to_atom(HighType),
             list_to_integer(HighDest)}
    end.
