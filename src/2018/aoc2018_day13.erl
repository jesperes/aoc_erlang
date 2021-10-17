-module(aoc2018_day13).


-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2018,
                day = 13,
                name = "Mine Cart Madness",
                expected = {{94, 78}, {26, 85}},
                has_input_file = true}.

-type input_type() :: {gb_trees:tree(), gb_trees:tree()}.
-type result_type() :: {integer(), integer()}.

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    xy_fold(fun store_pos/4, {gb_trees:empty(), gb_trees:empty()}, Input).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    do_steps(Input, part1).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    do_steps(Input, part2).

do_steps(Input, Part) ->
    {Carts, Tracks} = Input,
    case do_step(Carts, Tracks, Part) of
        {crash, {Y, X}} ->
            {X, Y};
        Carts0 ->
            case gb_trees:size(Carts0) of
                0 ->
                    no_carts_left;
                1 ->
                    {{Y, X}, _} = gb_trees:smallest(Carts0),
                    {X, Y};
                _ ->
                    do_steps({Carts0, Tracks}, Part)
            end
    end.

%% Move all carts one tick, return the set of new cart positions.
do_step(Carts, Tracks, Part) ->
    do_step0(Carts, gb_trees:empty(), Tracks, Part).

do_step0(Carts, MovedCarts, Tracks, Part) ->
    case gb_trees:is_empty(Carts) of
        true ->
            MovedCarts;
        false ->
            {Pos, Cart, RemainingCarts} = gb_trees:take_smallest(Carts),

            {Type, NewPos, RemainingCarts0, MovedCarts0} =
                move_cart(Pos, Cart, RemainingCarts, MovedCarts, Tracks),

            if (Type == crash) and (Part == part1) ->
                   %% For part 1, we simply return the first crash
                   %% position.
                   {crash, NewPos};
               true ->
                   %% For part 2, we continue with the crashed carts
                   %% removed.
                   do_step0(RemainingCarts0, MovedCarts0, Tracks, Part)
            end
    end.

move_cart(Pos, Cart, Remaining, Moved, Tracks) ->
    %% Remaining is the set of carts not yet moved in this round.
    %% Moved is the set of carts already moved in this round.
    {Dir, Turn} = Cart,

    NewPos = move(Pos, Dir),

    %% Either turn at an intersection, or follow the tracks.
    NewCart =
        case is_at_intersection(NewPos, Tracks) of
            true ->
                {turn(Dir, Turn), next_dir(Turn)};
            _ ->
                Track = gb_trees:get(NewPos, Tracks),
                {follow_track(Dir, Track), Turn}
        end,

    %% Insert the cart into the set of moved carts or remove carts if
    %% collided.
    case gb_trees:lookup(NewPos, Remaining) of
        {value, _} ->
            {crash, NewPos, gb_trees:delete(NewPos, Remaining), Moved};
        none ->
            case gb_trees:lookup(NewPos, Moved) of
                {value, _} ->
                    {crash, NewPos, Remaining, gb_trees:delete(NewPos, Moved)};
                none ->
                    {move, NewPos, Remaining, gb_trees:insert(NewPos, NewCart, Moved)}
            end
    end.

%%% Parser

%% Folds a fun over a binary representing a x,y-grid
xy_fold(Fun, Init, Binary) ->
    Str = binary_to_list(Binary),
    [First | _] = string:split(Str, "\n"),
    Width = length(First),

    {_, Out} =
        lists:foldl(fun(C, {N, AccIn}) ->
                       X = N rem (Width + 1),
                       Y = N div (Width + 1),
                       {N + 1, Fun(X, Y, C, AccIn)}
                    end,
                    {0, Init},
                    Str),
    Out.

store_pos(X, Y, C, {Carts, Tracks}) ->
    case pos_type(C) of
        cart ->
            {gb_trees:insert({Y, X}, {direction(C), -1}, Carts),
             gb_trees:insert({Y, X}, default_track_under_cart(C), Tracks)};
        track ->
            {Carts, gb_trees:insert({Y, X}, C, Tracks)};
        space ->
            {Carts, Tracks}
    end.

%%% Helpers

pos_type($<) ->
    cart;
pos_type($>) ->
    cart;
pos_type($^) ->
    cart;
pos_type($v) ->
    cart;
pos_type(32) ->
    space;
pos_type($\n) ->
    space;
pos_type($+) ->
    track;
pos_type($|) ->
    track;
pos_type($\\) ->
    track;
pos_type($/) ->
    track;
pos_type($-) ->
    track.

default_track_under_cart($<) ->
    $-;
default_track_under_cart($>) ->
    $-;
default_track_under_cart($^) ->
    $|;
default_track_under_cart($v) ->
    $|.

direction($^) ->
    0;
direction($>) ->
    1;
direction($v) ->
    2;
direction($<) ->
    3.

follow_track(0, $/) ->
    1; %% north turning right
follow_track(0, $|) ->
    0; %% north going straight
follow_track(0, $\\) ->
    3; %% north turning left
follow_track(1, $/) ->
    0; %% east turning left
follow_track(1, $-) ->
    1; %% east going straight
follow_track(1, $\\) ->
    2; %% east turning right
follow_track(2, $/) ->
    3; %% south turning right
follow_track(2, $|) ->
    2; %% south going straight
follow_track(2, $\\) ->
    1; %% south turning left
follow_track(3, $/) ->
    2; %% west turning left
follow_track(3, $-) ->
    3; %% west going straight
follow_track(3, $\\) ->
    0. %% west turning right

move({Y, X}, 0) ->
    {Y - 1, X};
move({Y, X}, 1) ->
    {Y, X + 1};
move({Y, X}, 2) ->
    {Y + 1, X};
move({Y, X}, 3) ->
    {Y, X - 1}.

next_dir(-1) ->
    0;
next_dir(0) ->
    1;
next_dir(1) ->
    -1.

turn(Dir, Turn) ->
    (Dir + Turn + 4) rem 4.

is_at_intersection(Pos, Tracks) ->
    case gb_trees:lookup(Pos, Tracks) of
        {value, $+} ->
            true;
        _ ->
            false
    end.
