%%% Advent of Code solution for 2019 day 25.
%%% Created: 2019-12-25T14:48:53+00:00

-module(aoc2019_day25).


-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 25,
                name = "Cryostasis",
                expected = 2098048,
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: intcode:intcode_program().
-type result_type() :: integer().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    intcode:parse(Binary).

-spec solve(Input :: input_type()) -> result_type().
solve(Input) ->
    part1(Input).

combinations([]) ->
    [];
combinations([H | T]) ->
    CT = combinations(T),
    [[H]] ++ [[H | L] || L <- CT] ++ CT.

part1(Prog) ->
    %% This was one of the most fun puzzles I've solved.
    %%
    %% First, I explored the maze by hand and keeping a hand-drawn map.
    %% The following items can be collected (for my input, that is):
    Items =
        ["fuel cell",
         "space heater",
         "hologram",
         "space law space brochure",
         "food ration",
         "tambourine",
         "spool of cat6",
         "festive hat"],

    Combinations = combinations(Items),

    %% These are the instructions needed to go fetch all items and bring
    %% them to the security checkpoing.
    BaseInstructions =
        lists:join("\n",
                   ["west", %% kitchen
                    "take hologram",
                    "north", %% stables
                    "take space heater",
                    "east", %% storage
                    "take space law space brochure",
                    "east", %% passages
                    "take tambourine",
                    "west", %% storage
                    "west", %% stables
                    "south", %% kitchen
                    "east", %% hull breach
                    "east", %% crew quarters
                    "take festive hat",
                    "east", %% observatory
                    "take food ration",
                    "east", %% engineering
                    "take spool of cat6",
                    "west", %% observatory
                    "west", %% crew quarters
                    "south", %% arcade
                    "east", %% gift wrapping"
                    "east", %% sick bay
                    "east"]) %% security checkpoint
        ++ "\n",

    %% Construct the input to the program which will collect all the
    %% items, then try all combinations of them until we find the right
    %% one.
    Instrs =
        lists:foldl(fun(Combination, Acc) ->
                       %% We could, of course, optimize a tiny bit here to avoid
                       %% dropping the items we are going to pick up, but the
                       %% entire program runs in < 1s anyway...
                       Acc
                       ++ lists:join("\n",
                                     %% Drop all items we might be holding
                                     ["drop spool of cat6",
                                      "drop food ration",
                                      "drop festive hat",
                                      "drop tambourine",
                                      "drop space law space brochure",
                                      "drop space heater",
                                      "drop hologram"])
                       ++ "\n"
                       ++ lists:map(fun(Item) -> "take " ++ Item ++ "\n" end, Combination)
                       ++ %% Uncomment this to show the inventory, but it is not
                          %% necessary when solving.
                          %% "inv\n"
                          %% Attempt to go through the security checkpoing. When we
                          %% have the right combination of items, the program will
                          %% terminate, at which point it will likely not have
                          %% processed all the inputs.
                          "south\n"
                    end,
                    BaseInstructions,
                    Combinations),

    %% Eventually we will find the combination
    %% - space heater
    %% - hologram
    %% - space law space brochure
    %% - spool of cat6
    %% Run the instructions
    {_, R} = intcode:execute(Prog, Instrs),

    %% Locate the keycode in the program output
    {match, [Code]} =
        re:run(
            lists:reverse(R),
            ".*You should be able to get in by typing (\\d+) on the keypad.*",
            [{capture, all_but_first, list}]),
    list_to_integer(Code).
