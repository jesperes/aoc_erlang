%%% Advent of Code solution for 2019 day 03.
%%% Created: 2019-12-03T15:32:24+00:00

-module(aoc2019_day03).


-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 3,
                name = "Crossed Wires",
                expected = {627, 13190},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: [string()].
-type result_type() :: {integer(), integer()}.

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    lists:map(fun(L) ->
                 lists:map(fun([C | Int]) -> {list_to_atom([C]), list_to_integer(Int)} end,
                           string:tokens(L, ","))
              end,
              string:tokens(binary_to_list(Input), "\n\r")).

-spec solve(Input :: input_type()) -> result_type().
solve([W1, W2]) ->
    InitState =
        {{0, 0}, %% Pos
         #{},    %% Grid ({X, Y} => {FirstWire, CombinedSignalDelay, IsIntersection})
         0},      %% SignalDelay

    %% The grid is represented as a map where the keys are positions,
    %% and the values are three-tuples: {FirstWire, SignalDelay,
    %% IsIntersection}. Once we have traced the paths of both wires, we
    %% filter out all the values which are intersections.
    Grid0 = trace_wire(W1, 1, InitState),
    Grid = trace_wire(W2, 2, {{0, 0}, Grid0, 0}),

    Xs = lists:filter(fun is_intersection/1, maps:to_list(Grid)),

    Part1 = map_smallest(fun({{X, Y}, _}) -> abs(X) + abs(Y) end, Xs),
    Part2 = map_smallest(fun({_, {_, SignalDelay, _}}) -> SignalDelay end, Xs),
    {Part1, Part2}.

is_intersection({_, {_, _, true}}) ->
    true;
is_intersection(_) ->
    false.

%% Map Fun over List and take the smallest element.
map_smallest(Fun, List) ->
    lists:min(
        lists:map(Fun, List)).

trace_wire([], _WireNum, {_, Grid, _}) ->
    Grid;
trace_wire([{Dir, Dist} | Wire], WireNum, State) ->
    Delta = delta(Dir),
    %% Trace a single wire segment
    S0 = trace_wire_segment(Delta, Dist, WireNum, State),
    trace_wire(Wire, WireNum, S0).

trace_wire_segment(_, 0, _, State) ->
    State;
trace_wire_segment({Dx, Dy} = Delta,
                   Dist,
                   WireNum,
                   {{X, Y} = _Pos, Grid, SignalDelay} = _State) ->
    NewPos = {X + Dx, Y + Dy},
    NewSignalDelay = SignalDelay + 1,

    NewGrid =
        case maps:is_key(NewPos, Grid) of
            true ->
                case maps:get(NewPos, Grid) of
                    {WireNum, _, _} ->
                        %% Move to next cell, updating position and signal delay
                        Grid;
                    {FirstWire, FirstSignalDelay, _} ->
                        %% Update signal delay, and mark this cell as an intersection
                        CombinedSignalDelay = FirstSignalDelay + NewSignalDelay,
                        maps:put(NewPos, {FirstWire, CombinedSignalDelay, true}, Grid)
                end;
            false ->
                %% Empty cell
                maps:put(NewPos, {WireNum, NewSignalDelay, false}, Grid)
        end,

    NewState = {NewPos, NewGrid, NewSignalDelay},
    trace_wire_segment(Delta, Dist - 1, WireNum, NewState).

delta('U') ->
    {0, -1};
delta('D') ->
    {0, 1};
delta('L') ->
    {-1, 0};
delta('R') ->
    {1, 0}.
