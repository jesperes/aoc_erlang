-module(aoc2017_day21).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 21,
                name = "Fractal Art",
                expected = {205, 3389823},
                has_input_file = true}.

-type rules() :: map().
-type input_type() :: rules().
-type result_type() :: integer().
-type image() :: [binary()].

-spec start_pattern() -> image().
start_pattern() ->
    [<<".#.">>, <<"..#">>, <<"###">>].

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    %% Split rules into rules for 2x2 blocks and rules for 3x3 blocks.
    lists:foldl(fun(Line, Rules) ->
                   case lists:map(fun list_to_binary/1, string:tokens(Line, "=>/ ")) of
                       [A1, A2, B1, B2, B3] -> maps:put([A1, A2], [B1, B2, B3], Rules);
                       [A1, A2, A3, B1, B2, B3, B4] ->
                           maps:put([A1, A2, A3], [B1, B2, B3, B4], Rules)
                   end
                end,
                #{},
                string:tokens(binary_to_list(Binary), "\n\r")).

-spec solve1(Input :: input_type()) -> result_type().
solve1(Rules) ->
    do_iterations(start_pattern(), Rules, 5).

-spec solve2(Input :: input_type()) -> result_type().
solve2(Rules) ->
    do_iterations(start_pattern(), Rules, 18).

do_iterations(Image, _Rules, 0) ->
    count_pixels(Image);
do_iterations(Image, Rules, N) ->
    Images = split(Image),
    EnhancedImages = enhance(Images, Rules),
    NewImage = stitch_square(EnhancedImages),
    do_iterations(NewImage, Rules, N - 1).

count_pixels([]) ->
    0;
count_pixels([Binary | Rest]) ->
    length(binary:matches(Binary, <<"#">>)) + count_pixels(Rest).

%% Image is a list of binaries, one per line of pixels
split(Image) when length(Image) rem 2 == 0 ->
    Lines = lists:map(fun(Line) -> split_binary_2(Line) end, Image),
    lists:append(split2(Lines));
split(Image) when length(Image) rem 3 == 0 ->
    Lines = lists:map(fun(Line) -> split_binary_3(Line) end, Image),
    lists:append(split3(Lines)).

split2([]) ->
    [];
split2([L1, L2 | Rest]) ->
    [zip(L1, L2) | split2(Rest)].

split3([]) ->
    [];
split3([L1, L2, L3 | Rest]) ->
    [zip3(L1, L2, L3) | split3(Rest)].

zip([], []) ->
    [];
zip([X1 | L1], [X2 | L2]) ->
    [[X1, X2] | zip(L1, L2)].

zip3([], [], []) ->
    [];
zip3([X1 | L1], [X2 | L2], [X3 | L3]) ->
    [[X1, X2, X3] | zip3(L1, L2, L3)].

-spec enhance(Images :: [image()], Rules :: rules()) -> [image()].
enhance(Images, Rules) ->
    [apply_rule(Image, Rules) || Image <- Images].

apply_rule(Image, Rules) ->
    apply_rule_on_rotation(lists:usort(rotations(Image)), Rules).

apply_rule_on_rotation([Image | Rest], Rules) ->
    case maps:get(Image, Rules, undefined) of
        undefined ->
            apply_rule_on_rotation(Rest, Rules);
        Match ->
            Match
    end.

%% Stitch two images together side by side.
stitch(Image1, Image2) ->
    lists:map(fun({A, B}) -> <<A/binary, B/binary>> end, lists:zip(Image1, Image2)).

%% Stitch a number of images together side by side
stitch_images([Image | Images]) ->
    lists:foldl(fun(Im, Acc) -> stitch(Acc, Im) end, Image, Images).

%% Stitch together a list of images into a square.
stitch_square(Images) ->
    Size = trunc(math:sqrt(length(Images))),
    lists:append(stitch_square(Images, Size)).

stitch_square([], _) ->
    [];
stitch_square(Images, Size) ->
    {FirstRow, Rest} = lists:split(Size, Images),
    [stitch_images(FirstRow) | stitch_square(Rest, Size)].

rotations([<<A, B>>, <<C, D>>] = Image) ->
    [Image,
     [<<C, A>>, <<D, B>>],
     [<<D, C>>, <<B, A>>],
     [<<B, D>>, <<A, C>>],
     [<<B, A>>, <<D, C>>],
     [<<D, B>>, <<C, A>>],
     [<<C, D>>, <<A, B>>],
     [<<A, C>>, <<B, D>>]];
rotations([<<A, B, C>>, <<D, E, F>>, <<G, H, I>>] = Image) ->
    [Image,
     [<<G, D, A>>, <<H, E, B>>, <<I, F, C>>],
     [<<I, H, G>>, <<F, E, D>>, <<C, B, A>>],
     [<<C, F, I>>, <<B, E, H>>, <<A, D, G>>],
     [<<C, B, A>>, <<F, E, D>>, <<I, H, G>>],
     [<<I, F, C>>, <<H, E, B>>, <<G, D, A>>],
     [<<G, H, I>>, <<D, E, F>>, <<A, B, C>>],
     [<<A, D, G>>, <<B, E, H>>, <<C, F, I>>]].

%% Split a binary into a list of two-byte binaries
split_binary_2(<<>>) ->
    [];
split_binary_2(<<X, Y, Rest/binary>>) ->
    [<<X, Y>> | split_binary_2(Rest)].

%% Split a binary into a list of three-byte binaries
split_binary_3(<<>>) ->
    [];
split_binary_3(<<X, Y, Z, Rest/binary>>) ->
    [<<X, Y, Z>> | split_binary_3(Rest)].
