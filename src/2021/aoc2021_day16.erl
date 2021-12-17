-module(aoc2021_day16).

-behavior(aoc_puzzle).

-export([parse/1, solve/1, info/0]).

-include("aoc_puzzle.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2021,
                day = 16,
                name = "Packet Decoder",
                expected = {927, 1725277876501},
                has_input_file = true,
                use_one_solver_fun = true}.

-type input_type() :: binary().
-type result_type() :: any().

-spec parse(Binary :: binary()) -> input_type().
parse(Binary) ->
    binary:decode_hex(binary:part(Binary, {0, size(Binary) - 1})).

-spec solve(Input :: input_type()) -> result_type().
solve(Binary) ->
    {Packets, _} = parse_packet(Binary),
    {version_sum(Packets), packet_value(Packets)}.

-spec parse_packet(Bitstring :: bitstring()) -> {Packet :: tuple(), Rest :: bitstring()}.
parse_packet(<<Version:3, TypeID:3, LiteralValue/bitstring>>) when TypeID =:= 4 ->
    {Literal, Rest} = parse_literal_value(LiteralValue, <<>>),
    {{literal, Version, TypeID, bitstring_to_decimal(Literal, 0)}, Rest};
parse_packet(<<Version:3,
               TypeID:3,
               LengthTypeID:1,
               TotalBitLength:15,
               Payload:TotalBitLength/bitstring,
               Rest0/bitstring>>)
    when LengthTypeID =:= 0 ->
    Packets = parse_packets(Payload, []),
    {{op, Version, TypeID, Packets}, Rest0};
parse_packet(<<Version:3, TypeID:3, LengthTypeID:1, NumSubPackets:11, Payload/bitstring>>)
    when LengthTypeID =:= 1 ->
    {Packets, Rest} = parse_n_packets(Payload, NumSubPackets, []),
    {{op, Version, TypeID, Packets}, Rest}.

parse_packets(<<>>, Acc) ->
    lists:reverse(Acc);
parse_packets(Binary, Acc) when is_bitstring(Binary) ->
    {Packet, Rest} = parse_packet(Binary),
    parse_packets(Rest, [Packet | Acc]).

parse_n_packets(Rest, 0, Acc) ->
    {lists:reverse(Acc), Rest};
parse_n_packets(Binary, N, Acc) ->
    {Packet, Rest} = parse_packet(Binary),
    parse_n_packets(Rest, N - 1, [Packet | Acc]).

parse_literal_value(<<Prefix:1, Group:4/bitstring, Rest/bitstring>>, Acc)
    when Prefix =:= 1 ->
    parse_literal_value(Rest, <<Acc/bitstring, Group:4/bitstring>>);
parse_literal_value(<<Prefix:1, Group:4, Rest/bitstring>>, Acc) when Prefix =:= 0 ->
    {<<Acc/bitstring, Group:4>>, Rest}.

version_sum(Packets) when is_list(Packets) ->
    lists:sum(
        lists:map(fun version_sum/1, Packets));
version_sum({op, Version, _TypeID, Packets}) ->
    Version + version_sum(Packets);
version_sum({literal, Version, _TypeID, _Literal}) ->
    Version.

packet_value({literal, _Version, _TypeID, Literal}) ->
    Literal;
packet_value({op, _Version, TypeID, Packets}) when TypeID =:= 0 ->
    lists:sum(packet_values(Packets));
packet_value({op, _Version, TypeID, Packets}) when TypeID =:= 1 ->
    lists:foldl(fun erlang:'*'/2, 1, packet_values(Packets));
packet_value({op, _Version, TypeID, Packets}) when TypeID =:= 2 ->
    lists:min(packet_values(Packets));
packet_value({op, _Version, TypeID, Packets}) when TypeID =:= 3 ->
    lists:max(packet_values(Packets));
packet_value({op, _Version, TypeID, [P1, P2]}) when TypeID >= 5 ->
    V1 = packet_value(P1),
    V2 = packet_value(P2),
    case TypeID of
        5 when V1 > V2 ->
            1;
        6 when V1 < V2 ->
            1;
        7 when V1 == V2 ->
            1;
        _ ->
            0
    end.

packet_values(Packets) ->
    lists:map(fun packet_value/1, Packets).

bitstring_to_decimal(<<>>, Acc) ->
    Acc;
bitstring_to_decimal(<<1:1, Rest/bitstring>>, Acc) ->
    bitstring_to_decimal(Rest, Acc bsl 1 bor 1);
bitstring_to_decimal(<<0:1, Rest/bitstring>>, Acc) ->
    bitstring_to_decimal(Rest, Acc bsl 1).
