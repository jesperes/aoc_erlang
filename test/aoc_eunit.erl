-module(aoc_eunit).

-include_lib("eunit/include/eunit.hrl").

find_changed_modules() ->
    lists:sort(
        lists:filtermap(fun(Line) ->
                           case re:run(Line,
                                       "(M|\\?\\?).*/(aoc\\d+_day\\d+)\\.erl$",
                                       [{capture, all_but_first, list}])
                           of
                               nomatch -> false;
                               {match, [_, ModStr]} -> {true, list_to_atom(ModStr)}
                           end
                        end,
                        string:tokens(
                            os:cmd("git status -u -s"), "\r\n"))).

%% Generate a eunit test case for all the modules which implement the
%% aoc_puzzle behavior.
aoc_test_() ->
    %% Automatically run all puzzles. Start running puzzles for the current
    %% year starting on december 1st.
    FirstYear = 2015,
    {ThisYear, ThisMonth, _} = erlang:date(),

    case find_changed_modules() of
        [] ->
            [{integer_to_list(Year),
              [begin
                   Module =
                       list_to_atom(lists:flatten(
                                        io_lib:format("aoc~w_day~2..0w", [Year, Day]))),
                   try
                       aoc_puzzle:mktest(aoc_puzzle:info(Module))
                   catch
                       error:undef ->
                           aoc_puzzle:mktest(Day)
                   end
               end
               || Day <- lists:seq(1, 25)]}
             || Year <- lists:seq(FirstYear, ThisYear),
                %% Start enumerating this year's puzzles on dec 1st.
                Year < ThisYear orelse ThisYear == Year andalso ThisMonth == 12];
        ChangedModules ->
            %% Some modules have local modifications, run them only.
            PuzzleInfos = [M:info() || M <- ChangedModules],
            {"Locally edited modules", lists:map(fun aoc_puzzle:mktest/1, PuzzleInfos)}
    end.
