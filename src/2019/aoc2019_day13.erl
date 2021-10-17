-module(aoc2019_day13).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 13,
                name = "Care Package",
                expected = {236, 11040},
                has_input_file = true}.

-type input_type() :: intcode:intcode_program().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    intcode:parse(Binary).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    part1(Input).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    part2(Input).

%%-define(EMPTY, 0).
%%-define(WALL, 1).
%%-define(BLOCK, 2).
-define(PADDLE, 3).
-define(BALL, 4).

%% Puzzle solution
part1(Prog) ->
    {_, Output} = intcode:execute(Prog),
    count_blocks(lists:reverse(Output), #{}).

count_blocks([], Map) ->
    maps:size(Map);
count_blocks([X, Y, 2 | Rest], Map) ->
    count_blocks(Rest, maps:put({X, Y}, 1, Map));
count_blocks([_, _, _ | Rest], Map) ->
    count_blocks(Rest, Map).

%% ---- [ Part 2 ] ----

-record(state,
        {score = 0,
         output = {},
         grid = #{},
         paddle_x = 0, %% x coord of paddle
         ball_x = 0}).   %% x coord of ball

part2(Prog) ->
    Prog0 = maps:merge(Prog, #{0 => 2}),
    InitState = #state{},
    {_, #state{score = Score}} =
        intcode:execute(Prog0, fun joystick/1, fun display/2, InitState),
    Score.

joystick(#state{ball_x = Bx, paddle_x = Px} = State) ->
    {State,
     case Bx - Px of
         0 ->
             0;
         D when D < 0 ->
             -1;
         _ ->
             1
     end}.

%% Receive X coordinate
display(X, #state{output = {}} = State) ->
    State#state{output = {X}};
%% Receive Y coordinate
display(Y, #state{output = {X}} = State) ->
    State#state{output = {X, Y}};
%% Receive score
display(Score, #state{output = {-1, 0}} = State) ->
    State#state{output = {}, score = Score};
%% Receive tile
display(Tile, #state{output = {X, Y}, grid = Grid} = State) ->
    State0 = State#state{grid = maps:put({X, Y}, Tile, Grid), output = {}},
    case Tile of
        ?BALL ->
            State0#state{ball_x = X};
        ?PADDLE ->
            State0#state{paddle_x = X};
        _ ->
            State0
    end.
