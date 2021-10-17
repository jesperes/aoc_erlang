-module(aoc2015_day06).

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 6,
                name = "Probably a Fire Hazard",
                expected = {543903, 14687245},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: [nonempty_string()].
-type result_type() :: {any(), any()}.

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    string:tokens(binary_to_list(Input), "\n\r").

-spec solve(Lines :: input_type()) -> result_type().
solve(Lines) ->
    Instrs =
        lists:map(fun(Line) ->
                     case string:tokens(Line, " ,") of
                         ["toggle", X0, Y0, "through", X1, Y1] ->
                             {toggle, {toi(X0), toi(Y0)}, {toi(X1), toi(Y1)}};
                         ["turn", "on", X0, Y0, "through", X1, Y1] ->
                             {turn_on, {toi(X0), toi(Y0)}, {toi(X1), toi(Y1)}};
                         ["turn", "off", X0, Y0, "through", X1, Y1] ->
                             {turn_off, {toi(X0), toi(Y0)}, {toi(X1), toi(Y1)}}
                     end
                  end,
                  Lines),

    {543903, 14687245} = process_instr(Instrs).

toi(N) ->
    list_to_integer(N).

process_instr(Instrs) ->
    process_instr(Instrs, grid_new()).

process_instr([], Grid) ->
    grid_get_solution(Grid);
process_instr([{toggle, From, To} | Rest], Grid) ->
    process_instr(Rest, fold_xy(fun grid_toggle/2, Grid, From, To));
process_instr([{turn_on, From, To} | Rest], Grid) ->
    process_instr(Rest, fold_xy(fun grid_turn_on/2, Grid, From, To));
process_instr([{turn_off, From, To} | Rest], Grid) ->
    process_instr(Rest, fold_xy(fun grid_turn_off/2, Grid, From, To)).

fold_xy(Fun, Init, {X0, Y0}, {X1, Y1}) ->
    lists:foldl(Fun,
                Init,
                [1 + X + Y * 1000 || X <- lists:seq(X0, X1), Y <- lists:seq(Y0, Y1)]).

%% OTP 21 has a new 'counters' module which can be used as a array of
%% 64-bit ints with destructive updates. Note, however, that indexing
%% starts at 1, not 0.

grid_new() ->
    {counters:new(1000 * 1000, []), %% on/off
     counters:new(1000 * 1000, [])}. %% brightness

grid_get_solution({A1, _} = Grid) ->
    #{size := Size} = counters:info(A1),
    count(Grid, 1, Size, {0, 0}).

count(_, N, N, Acc) ->
    Acc;
count({A1, A2} = Grid, Index, Size, {Acc0, Acc1}) ->
    count(Grid,
          Index + 1,
          Size,
          {Acc0 + counters:get(A1, Index), Acc1 + counters:get(A2, Index)}).

grid_toggle(Index, {A1, A2}) ->
    counters:put(A1,
                 Index,
                 case counters:get(A1, Index) of
                     0 ->
                         1;
                     1 ->
                         0
                 end),
    counters:add(A2, Index, 2),
    {A1, A2}.

grid_turn_on(Index, {A1, A2}) ->
    counters:put(A1, Index, 1),
    counters:add(A2, Index, 1),
    {A1, A2}.

grid_turn_off(Index, {A1, A2}) ->
    counters:put(A1, Index, 0),
    case counters:get(A2, Index) of
        N when N >= 1 ->
            counters:sub(A2, Index, 1);
        _ ->
            counters:put(A2, Index, 0)
    end,
    {A1, A2}.
