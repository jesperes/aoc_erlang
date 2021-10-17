-module(digest).

-export([digest_to_hexstring/1
        ]).


%% Convert a binary digest (i.e. output from erlang:md5/1) to a
%% lower-case hexadecimal string binary.
-spec digest_to_hexstring(binary()) -> binary().
digest_to_hexstring(Binary) ->
  << << (if N =< 9 -> N + $0;
            true -> N + $a - 10
         end):8 >> || <<N:4>> <= Binary >>.
