%% aoc_puzzle.hrl

-type aoc_puzzle_status() :: {boolean() | unknown, boolean() | unknown}.

-record(aoc_puzzle,
        {module :: module(),
         year :: integer(),
         day :: integer(),
         name = "" :: string(),
         has_input_file = true :: boolean(),
         use_one_solver_fun = false :: boolean(),
         expected = undefined :: {term(), term()} | term()}).

-type aoc_puzzle() :: #aoc_puzzle{}.
-type aoc_puzzle_id() :: {integer(), integer()}.
-type aoc_puzzle_map() :: #{aoc_puzzle_id() => aoc_puzzle()}.
