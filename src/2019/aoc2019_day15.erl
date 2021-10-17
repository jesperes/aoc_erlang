%%% Advent of Code solution for 2019 day 15.
%%% Created: 2019-12-15T10:32:09+00:00

-module(aoc2019_day15).

-include_lib("stdlib/include/assert.hrl").

-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 15,
                name = "Oxygen System",
                expected = {214, 344},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: any().
-type result_type() :: {integer(), integer()}.

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    intcode:parse(Binary).

-spec solve(Input :: input_type()) -> result_type().
solve(Prog) ->
    %% TODO cleanup; this throw thing to escape out of the first search is
    %% a little yucky. We should be able to do both part 1 and 2 in the
    %% same function.
    Pid = start_intcode(Prog),
    {Dist0, Grid0} =
        try
            explore(Pid,
                    #{pos => {0, 0},
                      dist => 0,
                      oxygen => not_found})
        catch
            {oxygen, Dist, Grid} ->
                {Dist, Grid}
        end,

    Grid1 = fill_oxygen(Pid, Grid0),
    OxygenDists =
        lists:filtermap(fun ({{_, _}, V}) ->
                                {true, V};
                            (_) ->
                                false
                        end,
                        maps:to_list(Grid1)),

    {Dist0, lists:max(OxygenDists)}.

start_intcode(Prog) ->
    Parent = self(),
    spawn(fun() ->
             intcode:execute(Prog,
                             fun(State) -> {State, receive Input -> Input end} end,
                             fun(Output, State) ->
                                State ! Output,
                                State
                             end,
                             Parent)
          end).

step_repair_robot(Dir, IntCodePid) ->
    IntCodePid ! Dir,
    receive
        Output ->
            Output
    end.

explore(Pid, Grid0) ->
    lists:foldl(fun(Dir, #{pos := Pos, dist := Dist} = Grid) ->
                   NewPos = move(Dir, Pos),
                   NewDist = Dist + 1,
                   case maps:is_key(NewPos, Grid) of
                       true -> Grid;
                       false ->
                           case step_repair_robot(Dir, Pid) of
                               0 -> maps:put(NewPos, $#, Grid); %% Wall
                               1 ->
                                   %% New pos is space. Robot was moved.
                                   NewGrid =
                                       explore(Pid,
                                               maps:merge(Grid,
                                                          #{NewPos => $.,
                                                            dist => NewDist,
                                                            pos => NewPos})),

                                   %% We must move the robot back after exploring; it has
                                   %% its own state in the intcode machine and does not
                                   %% automatically rewind with our stack.
                                   ?assertEqual(1, step_repair_robot(opposite(Dir), Pid)),
                                   maps:merge(NewGrid, #{pos => Pos, dist => Dist});
                               2 ->
                                   %% Found oxygen source. Throw to wind back up (we want
                                   %% to leave the intcode computer as is, and continue
                                   %% with the oxygen-filling).
                                   throw({oxygen,
                                          NewDist,
                                          #{pos => NewPos, %% Where we are
                                            NewPos => 0,   %% Time to fill oxygen
                                            time => 0}})
                           end
                   end
                end,
                Grid0,
                [1, 2, 3, 4]).

fill_oxygen(Pid, Grid0) ->
    lists:foldl(fun(Dir, #{pos := Pos, time := Time} = Grid) ->
                   NewPos = move(Dir, Pos),
                   NewTime = Time + 1,
                   case maps:is_key(NewPos, Grid) of
                       true ->
                           T = maps:get(NewPos, Grid),
                           if Time < T -> maps:put(NewPos, Time, Grid);
                              true -> Grid
                           end;
                       false ->
                           case step_repair_robot(Dir, Pid) of
                               0 -> maps:put(NewPos, $#, Grid); %% Wall
                               1 ->
                                   %% New pos is space. Robot was moved.
                                   NewGrid =
                                       fill_oxygen(Pid,
                                                   maps:merge(Grid,
                                                              #{NewPos => NewTime,
                                                                time => NewTime,
                                                                pos => NewPos})),

                                   step_repair_robot(opposite(Dir), Pid),
                                   maps:merge(NewGrid, #{pos => Pos, time => Time})
                           end
                   end
                end,
                Grid0,
                [1, 2, 3, 4]).

move(1, {X, Y}) ->
    {X, Y - 1}; %% north
move(2, {X, Y}) ->
    {X, Y + 1}; %% south
move(3, {X, Y}) ->
    {X - 1, Y}; %% west
move(4, {X, Y}) ->
    {X + 1, Y}. %% east

opposite(1) ->
    2;
opposite(2) ->
    1;
opposite(3) ->
    4;
opposite(4) ->
    3.
