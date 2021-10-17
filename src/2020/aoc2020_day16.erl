%%%=============================================================================
%%% @doc Advent of code puzzle solution
%%% @end
%%%=============================================================================
-module(aoc2020_day16).

-behavior(aoc_puzzle).

-export([ parse/1
        , solve1/1
        , solve2/1
        , info/0
        ]).

-include("aoc_puzzle.hrl").

%%------------------------------------------------------------------------------
%% @doc info/0
%% Returns info about this puzzle.
%% @end
%%------------------------------------------------------------------------------
-spec info() -> aoc_puzzle().
info() ->
  #aoc_puzzle{ module = ?MODULE
             , year = 2020
             , day = 16
             , name = "Ticket Translation"
             , expected = {27850, 491924517533}
             , has_input_file = true
             }.

%%==============================================================================
%% Types
%%==============================================================================
-type ticket() :: [FieldNum :: integer()].
-type range() :: { A1 :: integer()
                 , A2 :: integer()
                 , B1 :: integer()
                 , B2 :: integer()
                 }.
-type fields() :: #{atom() => range()}.
-type input_type() :: #{nearbytickets => [ticket()],
                        myticket => ticket(),
                        fields => fields()}.
-type result1_type() :: integer().
-type result2_type() :: result1_type().

%%------------------------------------------------------------------------------
%% @doc parse/1
%% Parses input file.
%% @end
%%------------------------------------------------------------------------------
-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
  {ok, FieldPattern} = re:compile("([a-z ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)"),
  Lines = string:tokens(binary_to_list(Input), "\r\n"),
  Map =
    lists:foldl(
      fun("your ticket:", #{section := fields} = Acc) ->
          maps:update(section, myticket, Acc);
         ("nearby tickets:", #{section := myticket} = Acc) ->
          maps:update(section, nearbytickets, Acc);
         (L, #{section := fields} = Acc) ->
          case re:run(L, FieldPattern,
                      [{capture, all_but_first, list}]) of
            {match, [F, A1, A2, B1, B2]} ->
              Range = {{stoi(A1), stoi(A2)}, {stoi(B1), stoi(B2)}},
              F0 = list_to_atom(F),
              maps:update_with(
                fields,
                fun(Old) -> maps:put(F0, Range, Old) end,
                #{F0 => Range}, Acc)
          end;
         (L, #{section := myticket} = Acc) ->
          maps:put(myticket, str_to_int_list(L), Acc);
         (L, #{section := nearbytickets} = Acc) ->
          Nums = str_to_int_list(L),
          maps:update_with(
            nearbytickets,
            fun(Old) -> [Nums|Old] end, [Nums], Acc)
      end, #{section => fields}, Lines),
  maps:remove(section, Map).

%%------------------------------------------------------------------------------
%% @doc solve1/1
%% Solves part 1. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve1(Input :: input_type()) -> result1_type().
solve1(#{fields := Fields,
         nearbytickets := Tickets}) ->

  lists:foldl(
    fun(Num, Acc) ->
        case is_valid_for_some_field(Num, Fields) of
          true -> Acc;
          false -> Num + Acc
        end
    end, 0, lists:flatten(Tickets)).

%%------------------------------------------------------------------------------
%% @doc solve2/1
%% Solves part 2. Receives parsed input as returned from parse/1.
%% @end
%%------------------------------------------------------------------------------
-spec solve2(Input :: input_type()) -> result2_type().
solve2(#{myticket := MyTicket}) ->

  %% This list was obtained by calling `show_valid_fields/1' and
  %% then matching up the valid fields by ocular inspection.
  DepartureFields = [3, 4, 8, 11, 12, 14],

  %% Multiply all the departure fields on our ticket
  lists:foldl(fun erlang:'*'/2, 1,
              lists:map(fun(Pos) ->
                            lists:nth(Pos, MyTicket)
                        end, DepartureFields)).

%%==============================================================================
%% Internals
%%==============================================================================

-compile([{nowarn_unused_function, [ show_valid_fields/1
                                   , find_field/3
                                   , is_ticket_valid/2
                                   ]}]).

%% Prints out valid fields.
show_valid_fields(#{fields := Fields,
                    myticket := MyTicket,
                    nearbytickets := Tickets}) ->
  ValidTickets =
    lists:reverse(
      lists:filter(fun(Ticket) -> is_ticket_valid(Ticket, Fields) end,
                   Tickets)),

  lists:foreach(
    fun(Pos) ->
        ValidFieldsForPos =
          find_field(Pos, [MyTicket|ValidTickets], Fields),
        io:format("~nValid fields for pos ~p~n~p~n", [Pos, ValidFieldsForPos])
    end, lists:seq(1, maps:size(Fields))).

%% Find what field corresponds to position `Pos'
find_field(Pos, Tickets, Fields) ->
  FieldVals =
    lists:map(fun(Ticket) ->
                  lists:nth(Pos, Ticket)
              end, Tickets),

  maps:filter(fun(_Field, Range) ->
                  lists:all(fun(V) -> in_range(V, Range) end,
                            FieldVals)
              end, Fields).


%% Valid tickets are tickets where no fields are invalid, i.e.  all
%% numbers has a field they can belong to.
is_ticket_valid(Ticket, Fields) ->
  lists:all(
    fun(Num) ->
        is_valid_for_some_field(Num, Fields)
    end, Ticket).

%% Determine if a given number is valid, i.e. if there is a field
%% which this number could belong to.
is_valid_for_some_field(Num, Fields) ->
  lists:any(
    fun(Range) ->
        in_range(Num, Range)
    end, maps:values(Fields)).

in_range(Num, {{A1, A2}, {B1, B2}}) ->
  ((Num >= A1) and (Num =< A2))
    orelse ((Num >= B1) and (Num =< B2)).

%% ======================================================================
%% Helpers
%% ======================================================================

str_to_int_list(S) ->
  lists:map(fun list_to_integer/1, string:split(S, ",", all)).

stoi(S) ->
   list_to_integer(S).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
