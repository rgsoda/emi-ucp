-module(ucp_utils).
-author('rafal.galczynski@jtendo.com').

-export([to_ira/1, to_7bit/1, calculate_sender/1,
         fill_with_zeros/1, compose_message/2]).

-define(STX,16#02).
-define(ETX,16#03).


%% ---------------------------------------------------------------------
%% Function for converting string to string
%% encoded into IRA, after GSM 03.38 Version 5.3.0
%% ---------------------------------------------------------------------
%% Initial Function call: to_ira(String).
%% ---------------------------------------------------------------------
%% Input:  String containing only ASCII characters
%% ---------------------------------------------------------------------
%% Output: String converted to IRA
%% ---------------------------------------------------------------------
to_ira(Str) ->
    GsmMessage = lists:map(
                  fun(X) -> ucp_ia5:ascii_to_gsm(X) end, Str),
    lists:flatten(GsmMessage).

%% ---------------------------------------------------------------------
%% Function for converting string to 7-bit encoding according to:
%% GSM 03.38 Version 5.3.0
%% ---------------------------------------------------------------------
%% Initial Function call: to_7bit(String).
%% ---------------------------------------------------------------------
%% Input:  String containing only ASCII characters
%% ---------------------------------------------------------------------
%% Output: String coded to 7bit
%% ---------------------------------------------------------------------
to_7bit(Str) ->
    binary:bin_to_list(ucp_7bit:to_7bit(Str)).

%% ---------------------------------------------------------------------
%% Function for calculating UCP OAdC field for string and returns list
%% of Hex octets
%% ---------------------------------------------------------------------
%% Initial Function call: calculate_sender(String).
%% ---------------------------------------------------------------------
%% Input:  String containing only ASCII characters
%% ---------------------------------------------------------------------
%% Output: tuple { otoa, String, sender, String }
%% ---------------------------------------------------------------------
calculate_sender(Sender) ->
    case has_only_digits(Sender) of
        true ->
            { otoa, "1139", sender, Sender};
        false ->
            { otoa, "5039", sender, hex:list_to_hexstr(
                                      append_length(
                                        to_7bit(
                                          to_ira(Sender))))}
    end.


%% ---------------------------------------------------------------------
%% Function for composing whole ucp message
%% ---------------------------------------------------------------------
%% Initial Function call: compose_message(#header, #ucp5x).
%% ---------------------------------------------------------------------
%% Input:  (#header, #ucp5x)
%% ---------------------------------------------------------------------
%% Output: complete ucp message
%% ---------------------------------------------------------------------
compose_message(Header, Body) ->
    BodyFields = lists:nthtail(1,tuple_to_list(Body)),
    HeaderFields = lists:nthtail(1,tuple_to_list(Header)),
    UcpMessage = string:join(HeaderFields,"/") ++"/"++ string:join(BodyFields,"/"),
    UcpMessageStr = binary:bin_to_list(binary:list_to_bin(UcpMessage)),
    CRC = calculate_crc(UcpMessageStr),
    CompleteUcpMessage =  UcpMessageStr++CRC,
    <<?STX,CompleteUcpMessage,?ETX>>.



append_length(L) ->
    lists:flatten([length(L), L]).

fill_with_zeros(Value) ->
    lists:flatten(io_lib:format("~5.5.0w",[Value])).

get_8lsb(Integer) ->
    Integer band 255.

calculate_crc(UcpMessage) ->
    hex:int_to_hexstr(
      get_8lsb(
        lists:sum(UcpMessage))).

is_digit(C) when C > 46, C < 58  -> true;
is_digit(_) -> false.

has_only_digits(Str) ->
    lists:all(fun(Elem) -> is_digit(Elem) end, Str).
