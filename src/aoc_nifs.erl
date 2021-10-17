-module(aoc_nifs).

-export([digest_to_hexstring/1, init/0]).

-on_load init/0.

%% Load NIF
init() ->
    File = filename:join(?NIF_DIR, "aoc_nifs.so"),
    ok =
        erlang:load_nif(
            filename:rootname(File), 0).

-spec digest_to_hexstring(Binary :: binary()) -> binary().
digest_to_hexstring(_Binary) ->
    erlang:nif_error(not_loaded).
