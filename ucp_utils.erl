-module(ucp_utils).
-author('rafal.galczynski@jtendo.com').
-include("../../include/ucp/ucp_syntax.hrl").
-include("../../include/logger.hrl").

-compile([debug_info]).

-export([
         to_ira/1,
         to_7bit/1,
         calculate_sender/1,
         fill_with_zeros/2,
         compose_message/2,
         unpackUCP/1,
         analyze_ucp_body/1
         %% create_ieia_00/3,
         %% create_ieia_05/2,
         %% create_udh/1,
         %% create_dcs_normal/0,
         %% create_dcs_binary/0
        ]).

-define(STX,16#02).
-define(ETX,16#03).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function for converting string to string
%% encoded into IRA, after GSM 03.38 Version 5.3.0
%%
%% @spec to_ira(String) -> String in ira alphabet
%% @end
%%--------------------------------------------------------------------

to_ira(Str) ->
    GsmMessage = lists:map(
                  fun(X) -> ucp_ia5:ascii_to_gsm(X) end, Str),
    lists:flatten(GsmMessage).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function for converting string to 7-bit encoding according to:
%% GSM 03.38 Version 5.3.0
%%
%% @spec to_7bit(String) -> String coded to 7bit
%% @end
%%--------------------------------------------------------------------

to_7bit(Str) ->
    binary:bin_to_list(ucp_7bit:to_7bit(Str)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function for calculating UCP OAdC field for string and returns list
%% of Hex octets
%%
%% @spec calculate_sender(String) -> {otoa, OTOA, sender, SENDER }
%% @end
%%--------------------------------------------------------------------

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

%% create_ieia_00(RefNo, Total, Actual) ->
%%     "0003" ++ hex:int_to_hexstr(RefNo) ++ hex:int_to_hexstr(Total)
%%         ++ hex:int_to_hexstr(Actual).

%% create_ieia_05(SourcePort, DestPort) ->
%%     "0504" ++ hex:int_to_hexstr(SourcePort) ++ hex:int_to_hexstr(DestPort).

%% create_udh(DDList) ->
%%     TT = "01",
%%     DD = lists:append(DDList),
%%     UDHL = trunc(length(DD)/2),
%%     LL = UDHL+1,
%%     UDH = lists:append([
%%                   TT,
%%                   hex:int_to_hexstr(LL),
%%                   hex:int_to_hexstr(UDHL),
%%                         DD]),
%%     UDH.

%% create_dcs_binary() ->
%%     "0201F5".

%% create_dcs_normal() ->
%%     "020100".


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function for composing whole ucp message
%%
%% @spec compose_message(#header, #body) -> <<Message>>
%% @end
%%--------------------------------------------------------------------
compose_message(Header, Body) ->
    BodyFields = lists:nthtail(1,tuple_to_list(Body)),
    HeaderFields = lists:nthtail(1,tuple_to_list(Header)),
    UcpMessage = lists:flatten(
                   string:join(HeaderFields,"/") ++"/"++
                       string:join(BodyFields,"/")),
    CRC = calculate_crc(UcpMessage),
    CompleteUcpMessage =  UcpMessage++CRC,
    ?APP_DEBUG("~p",[CompleteUcpMessage]),
    binary:list_to_bin([?STX,CompleteUcpMessage,?ETX]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function for composing whole ucp message
%%
%% @spec unpackUCP(<<UCPMessage>>) -> {#header, #body} |
%%                                    {error, wrong_message}
%% @end
%%--------------------------------------------------------------------
unpackUCP(Binary) ->
    Ucp = binary:bin_to_list(Binary,{1,byte_size(Binary)-2}),
    case list_to_tuple(re:split(Ucp,"/")) of
        {TRN, LEN, OR, OT, ACK, SM, CRC} ->
            Header = #header{trn=TRN, len=LEN, o_r=OR, ot=OT},
            Body = #ack{ack=ACK, sm=SM, crc=CRC},
            {Header, Body};
        {TRN, LEN, OR, OT, NACK, EC, SM, CRC} ->
            Header = #header{trn=TRN, len=LEN, o_r=OR, ot=OT},
            Body = #nack{nack=NACK, ec=EC, sm=SM,crc=CRC},
            {Header, Body};
        Other ->
            ?APP_DEBUG("~p ~p", ["Received WRONG MESSAGE",Other]),
            {error, wrong_message}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function for analyze upc body record
%%
%% @spec analyze_ucp_body(#body) -> {ack,ok} |
%%                                  {nack, system_message} |
%%                                  {error, unknow_response}
%% @end
%%--------------------------------------------------------------------

analyze_ucp_body(BodyRec) when is_record(BodyRec, ack) ->
    {ack,ok};

analyze_ucp_body(BodyRec) when is_record(BodyRec, nack) ->
    {nack, BodyRec#nack.sm};

analyze_ucp_body(_) ->
    {error, unknow_response}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function for appending list length to biggining of the list
%%
%% @spec append_length(List) -> [Length, List]
%% @end
%%--------------------------------------------------------------------

append_length(L) ->
    lists:flatten([length(L), L]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function for appending specified numer of "0" chars
%%
%% @spec fill_with_zeros(Value, NumerOfZeros) -> String
%% @end
%%--------------------------------------------------------------------

fill_with_zeros(Value, Zeros) when is_list(Value)->
    case string:len(Value) >= Zeros of
        true ->
            Value;
        false ->
            Diff = Zeros - string:len(Value),
            string:concat(string:chars($0, Diff),Value)
    end;

fill_with_zeros(Value, Zeros) when is_integer(Value)->
    StrZeros = integer_to_list(Zeros),
    StrFormat = "~"++StrZeros++"."++StrZeros++".0w",
    lists:flatten(
      io_lib:format(StrFormat,[Value])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function for getting 8 last significant bits of number
%%
%% @spec get_8lsb(Integer) -> Integer
%% @end
%%--------------------------------------------------------------------

get_8lsb(Integer) ->
    Integer band 255.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function for calculating CRC checksum for UCP Message
%%
%% @spec calculate_crc(Message) -> HexString
%% @end
%%--------------------------------------------------------------------

calculate_crc(UcpMessage) ->
    hex:int_to_hexstr(
      get_8lsb(
        lists:sum(UcpMessage))).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function for checking if Char is digit
%%
%% @spec is_digit(Char) -> true | false
%% @end
%%--------------------------------------------------------------------

is_digit(C) when C > 46, C < 58  -> true;
is_digit(_) -> false.

%% @private
%% @doc
%% Function for checking if String contains only digits
%%
%% @spec has_only_digits(String) -> true | false
%% @end
%%--------------------------------------------------------------------

has_only_digits(Str) ->
    lists:all(fun(Elem) -> is_digit(Elem) end, Str).

