%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created :  2 Nov 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(inputs).

-export([ get_as_binary/2
        , get_as_string/2
        , get_as_lines/2
        , get_as_ints/2
        , parse_lines/3
        ]).

-spec get_as_binary(Year :: integer(), Day :: integer()) -> binary().
get_as_binary(Year, Day) ->
  {ok, Bin} = file:read_file(get_input_filename(Year, Day)),
  Bin.

-spec get_as_string(Year :: integer(), Day :: integer()) -> string().
get_as_string(Year, Day) ->
  binary_to_string(get_as_binary(Year, Day)).

-spec get_as_lines(Year :: integer(), Day :: integer()) -> list(string()).
get_as_lines(Year, Day) ->
  string:tokens(get_as_string(Year, Day), "\n\r").

get_as_ints(Year, Day) ->
  lists:map(fun erlang:list_to_integer/1, get_as_lines(Year, Day)).

-spec parse_lines(Binary :: binary(),
                  Delims :: string(),
                  Fun :: fun()) ->
                     list(term()).
parse_lines(Binary, Delims, Fun) ->
  lists:map(fun(Line) ->
                Fun(string:tokens(Line, Delims))
            end,
            string:tokens(binary_to_string(Binary), "\n\r")).

%% Internal functions

%% Convert a binary to a flattened, trimmed list.
binary_to_string(Bin) ->
  string:trim(lists:flatten(binary_to_list(Bin))).

-spec get_input_dir() -> file:filename().
get_input_dir() ->
  filename:join(code:priv_dir(aoc_erlang), "inputs").

-spec get_input_filename(Year :: integer(), Day :: integer()) ->
                            file:filename().
get_input_filename(Year, Day) ->
  Filename =
    filename:join([get_input_dir(),
                   integer_to_list(Year),
                   io_lib:format("input~2..0B.txt", [Day])]),
  true = filelib:is_file(Filename),
  Filename.
