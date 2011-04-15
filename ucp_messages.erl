-module(ucp_messages).
-author('rafal.galczynski@jtendo.com').

-include("../../include/ucp/ucp_syntax.hrl").
-include("../../include/logger.hrl").

-export([create_m51/4, create_m60/3, create_m31/2, partition_by/2, analyze_message/1]).

-compile([debug_info]).

-define(BODY_LEN, 153).
-define(HEADER_LEN, 14).
-define(EMPTY_BODY_LEN_51, 40).
-define(EMPTY_BODY_LEN_60, 18).
-define(EMPTY_BODY_LEN_31, 8).

%% ---------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Function try to create UCP 51 Message
%%
%% @spec smsc_reconnect(Seq, Sender, Receiver, Message) -> {ok, UcpMsg}
%% @end
%%--------------------------------------------------------------------

create_m51(Seq, Sender, Receiver, Message) ->
    case analyze_message(Message) of
        {'7bit', true} ->
            create_m51_normal(Seq, Sender, Receiver, Message);
        {'7bit', false} ->
            create_m51_unicode(Seq, Sender, Receiver, Message)
    end.


%% ---------------------------------------------------------------------
%% Internals
%% ---------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function try to create UCP 51 Message having utf-8 chars
%%
%% @spec smsc_reconnect(Seq, Sender, Receiver, Message) -> {ok, UcpMsg}
%% @end
%%--------------------------------------------------------------------

create_m51_unicode(Seq, Sender, Receiver, Message) ->
    {otoa, OTOA, sender, UCPSender} = ucp_utils:calculate_sender(Sender),
    HexStr = hex:list_to_hexstr(
               unicode:characters_to_list(Message)),
    HexMessage = lists:flatten([ ucp_utils:fill_with_zeros(X,4) || X <- HexStr ]),

    XSER = "020108",
    NB = integer_to_list(trunc(length(Message)*16)),
    Body = #ucp5x{
      oadc=UCPSender,
      adc=Receiver,
      otoa=OTOA,
      mt = "4",
      nb = NB,
      xser = XSER,
      msg=HexMessage},

    MessageLen =
        length(UCPSender++Receiver++HexMessage++XSER++NB) +
        ?HEADER_LEN + ?EMPTY_BODY_LEN_51,

    Header = #header{
      trn=Seq,
      len=ucp_utils:fill_with_zeros(MessageLen,5),
      o_r="O",
      ot="51"},

    UcpMessage = ucp_utils:compose_message(Header, Body),
    {ok, UcpMessage}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function try to create UCP 51 Message not having utf-8 chars
%%
%% @spec smsc_reconnect(Seq, Sender, Receiver, Message) -> {ok, UcpMsg}
%% @end
%%--------------------------------------------------------------------

create_m51_normal(Seq, Sender, Receiver, Message) ->

    UCPMsg = hex:list_to_hexstr(
               ucp_utils:to_ira(Message)),
    {otoa, OTOA, sender, UCPSender} = ucp_utils:calculate_sender(Sender),

    Body = #ucp5x{
      oadc=UCPSender,
      adc=Receiver,
      otoa=OTOA,
      mt = "3",
      msg=UCPMsg},

    MessageLen = length(UCPSender++Receiver) +
        length(UCPMsg)*2 +
        ?HEADER_LEN +
        ?EMPTY_BODY_LEN_51,

    Header = #header{
      trn=Seq,
      len=ucp_utils:fill_with_zeros(MessageLen,5),
      o_r="O",
      ot="51"},

    UcpMessage = ucp_utils:compose_message(Header, Body),
    {ok, UcpMessage}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function try to create UCP 60 Message Login
%%
%% @spec smsc_reconnect(Seq, Sender, Receiver, Message) -> {ok, UcpMsg}
%% @end
%%--------------------------------------------------------------------

create_m60(Seq, Login,  Password) ->
    IRAPassword = hex:list_to_hexstr(ucp_utils:to_ira(Password)),
    STYP = "1",
    OTON = "6",
    ONPI = "5",
    Body = #ucp60{
      oadc=Login,
      oton=OTON,
      onpi=ONPI,
      styp=STYP,
      pwd = IRAPassword,
      vers = "0100"},

    MessageLen = length(IRAPassword)*2 +
        length(Login++STYP++OTON++ONPI) +
        ?HEADER_LEN + ?EMPTY_BODY_LEN_60,

    Header = #header{
      trn=Seq,
      len=ucp_utils:fill_with_zeros(MessageLen,5),
      o_r="O",
      ot="60"},

    UcpMessage = ucp_utils:compose_message(Header, Body),
    {ok, UcpMessage}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function try to create UCP 31 Alert / used as keep alive
%%
%% @spec smsc_reconnect(Seq, Sender, Receiver, Message) -> {ok, UcpMsg}
%% @end
%%--------------------------------------------------------------------

create_m31(Seq, Address) ->
    Body = #ucp31{
      adc=Address,
      pid = "0539"},

    MessageLen = length(Address) + ?HEADER_LEN + ?EMPTY_BODY_LEN_31,

    Header = #header{
      trn=Seq,
      len=ucp_utils:fill_with_zeros(MessageLen,5),
      o_r="O",
      ot="31"},

    UcpMessage = ucp_utils:compose_message(Header, Body),
    {ok, UcpMessage}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function splits list into list of lists each Num long
%%
%% @spec partition_by([List], Int) -> [[],[],...]
%% @end
%%--------------------------------------------------------------------

partition_by(L, Num)->
    partition_by(L,Num,[]).

partition_by(L, Num, Acc) when length(L) =< Num ->
    lists:reverse([L|Acc]);

partition_by(L, Num, Acc) when length(L) > Num ->
    {H, T} = lists:split(Num, L),
    partition_by(T,Num,[H|Acc]);

partition_by([], _Num, Acc)->
    lists:reverse(Acc).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function checks if String contains utf8 chars
%%
%% @spec analyze_message(String) -> {7bit, true} | {7bit, false}
%% @end
%%--------------------------------------------------------------------

analyze_message(Message) ->
    Bit = unicode:bin_is_7bit(unicode:characters_to_binary(Message)),
    {'7bit', Bit}.


