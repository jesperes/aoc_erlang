-module(utils).

-export([freq_map/1]).

freq_map(List) ->
    lists:foldl(fun(Elem, Acc) -> maps:update_with(Elem, fun(N) -> N + 1 end, 1, Acc) end,
                #{},
                List).
