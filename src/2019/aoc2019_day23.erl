%%% Advent of Code solution for 2019 day 23.
%%% Created: 2019-12-23T06:13:41+00:00

-module(aoc2019_day23).


-include("aoc_puzzle.hrl").

-export([parse/1, solve/1, info/0]).

-behavior(aoc_puzzle).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2019,
                day = 23,
                name = "Category Six",
                expected = {24268, 19316},
                use_one_solver_fun = true,
                has_input_file = true}.

-type input_type() :: intcode:intcode_program().
-type result_type() :: {integer(), integer()}.

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    intcode:parse(Binary).

-spec solve(Input :: input_type()) -> result_type().
solve(Prog) ->
    N = 50,
    Addrs = lists:seq(0, N - 1),
    spawn_nics(Prog, Addrs),
    spawn_nat(),
    wait_for_nic_boot(Addrs),
    start_nics(Addrs),
    Res = receive
              Result ->
                  Result
          end,
    kill_all([255 | Addrs]),
    Res.

-record(nic,
        {addr :: integer(),     % Network address
         booting :: boolean(),  % True when booting, false otherwise
         parent :: pid(),       % Parent pid
         outseq :: integer(),   % Next output is: 0 = addr, 1 = x, 2 = y
         outaddr ::
             integer() |
             undefined, % Address for next output package
         outx ::
             integer() |
             undefined, % Buffered outgoing X value
         iny ::
             integer() |
             undefined, % Buffered incoming Y value
         idle :: boolean() | undefined}).
-record(nat,
        {packet ::
             {integer(), integer()} |
             undefined,       % Last packet seen by NAT
         idle :: term(),      % Set of idle pids
         parent :: pid(),     % Parent pid
         solutions :: term(),
         first :: integer() | undefined}).

kill_all(L) ->
    lists:foreach(fun(N) ->
                     case nic_pid(N) of
                         undefined -> ok;
                         Pid -> exit(Pid, kill)
                     end
                  end,
                  L).

nic(Prog, Addr, Parent) ->
    register(nic_name(Addr), self()),
    intcode:execute(Prog,
                    fun nic_input/1,
                    fun nic_output/2,
                    #nic{addr = Addr,
                         booting = true,
                         parent = Parent,
                         outseq = 0}).

spawn_nat() ->
    Parent = self(),
    Pid = spawn(fun() -> nat(Parent) end),
    receive
        {nat_start, Pid} ->
            ok
    end,
    Pid.

-spec nat(Parent :: pid()) -> no_return().
nat(Parent) ->
    register(nic_name(255), self()),
    Parent ! {nat_start, self()},
    nat_loop(#nat{parent = Parent,
                  solutions = #{},
                  idle = sets:new()}).

check_mql() ->
    case process_info(self(), message_queue_len) of
        {message_queue_len, L} when L > 100 ->
            L;
        {message_queue_len, L} ->
            L
    end.

is_network_idle() ->
    lists:foldl(fun (_, false) ->
                        false;
                    (N, true) ->
                        Pid = nic_pid(N),
                        Pid ! {is_idle, self()},
                        receive
                            {idle, Pid, IsIdle} ->
                                IsIdle
                        end
                end,
                true,
                lists:seq(0, 49)).

nat_loop(#nat{parent = Parent,
              solutions = Solutions,
              first = First,
              packet = Packet} =
             State) ->
    MQL = check_mql(),

    receive
        P ->
            nat_loop(State#nat{packet = P})
    after 50 ->
        case MQL == 0 andalso is_tuple(Packet) andalso is_network_idle() of
            false ->
                nat_loop(State);
            true ->
                {_, Y} = Packet,
                nic_pid(0) ! Packet,
                State0 =
                    case First of
                        undefined ->
                            State#nat{first = Y};
                        _ ->
                            State
                    end,

                case maps:is_key(Y, Solutions) of
                    true ->
                        Parent ! {First, Y},
                        receive
                            killme ->
                                ok
                        end;
                    _ ->
                        nat_loop(State0#nat{solutions = maps:put(Y, true, Solutions),
                                            packet = undefined})
                end
        end
    end.

spawn_nics(Prog, Addrs) ->
    Parent = self(),
    lists:foldl(fun(N0, Acc) -> maps:put(N0, spawn(fun() -> nic(Prog, N0, Parent) end), Acc)
                end,
                #{},
                Addrs).

wait_for_nic_boot(Addrs) ->
    lists:foreach(fun(N0) -> receive {booted, N0} -> ok end end, Addrs).

start_nics(Addrs) ->
    lists:foreach(fun(N0) -> nic_pid(N0) ! all_booted end, Addrs).

nic_pid(Addr) ->
    whereis(nic_name(Addr)).

nic_name(Addr) ->
    list_to_atom(lists:flatten(
                     io_lib:format("nic-~p", [Addr]))).

%% "booting" means NIC is waiting for its network address.
nic_input(#nic{booting = true,
               addr = Addr,
               parent = Parent} =
              State) ->
    %% Signal parent that we have started.
    Parent ! {booted, Addr},
    %% Wait until all NICs have booted
    receive
        all_booted ->
            ok
    end,
    {State#nic{booting = false}, Addr};
%% If we have a buffered Y value, return it.
nic_input(#nic{iny = Y} = State) when is_integer(Y) ->
    {State#nic{iny = undefined}, Y};
%% Poll input queue
nic_input(State) ->
    check_mql(),
    %% This is the only place where the intcode VM actually
    %% waits.
    receive
        {is_idle, Pid} ->
            Pid ! {idle, self(), State#nic.idle},
            nic_input(State);
        {X, Y} ->
            %% Buffer the Y value so that we can return it in the next input
            %% instruction.
            %% erlang:display({ts(), received, {X, Y}}),
            {State#nic{iny = Y, idle = false}, X}
    after 0 ->
        %% Tell NAT that we are idle
        {State#nic{idle = true}, -1}
    end.

%% If all computers have empty incoming packet queues and are
%% continuously trying to receive packets without sending packets, the
%% network is considered idle.

%% New destination address
nic_output(Output, #nic{outseq = N} = State) when N rem 3 == 0 ->
    State#nic{outaddr = Output, outseq = N + 1};
%% Buffer the X value
nic_output(X, #nic{outseq = N} = State) when N rem 3 == 1 ->
    State#nic{outseq = N + 1, outx = X};
%% Send package
nic_output(Y,
           #nic{outseq = N,
                outx = X,
                outaddr = OutAddr} =
               State)
    when N rem 3 == 2 ->
    %% erlang:display({ts(), self(), sends, {X, Y}, to, OutAddr}),
    nic_pid(OutAddr) ! {X, Y},
    State#nic{outseq = N + 1,
              outx = undefined,
              outaddr = undefined,
              idle = false}.
