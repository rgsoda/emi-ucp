-module(ucp_messages).
-author('rafal.galczynski@jtendo.com').

-include("ucp_syntax.hrl").

-export([create_m51/3, create_m60/2]).

-define(HEADER_LEN, 14).
-define(EMPTY_BODY_LEN_51, 40).
-define(EMPTY_BODY_LEN_60, 21).


create_m51(Sender, Receiver, Message) ->
    UCPMsg = hex:list_to_hexstr(ucp_utils:to_ira(Message)),
    {otoa, OTOA, sender, UCPSender} = ucp_utils:calculate_sender(Sender),

    Body = #ucp5x{
      adc=Receiver,
      oadc=UCPSender,
      otoa=OTOA,
      msg=UCPMsg},

    MessageLen = length(UCPSender++Receiver++UCPMsg) + ?HEADER_LEN + ?EMPTY_BODY_LEN_51,

    Header = #header{
      trn="00",
      len=ucp_utils:fill_with_zeros(MessageLen),
      o_r="O",
      ot="51"},

    UcpMessage = ucp_utils:compose_message(Header, Body),
    {ok, UcpMessage}.

create_m60(Login, Password) ->
    IRAPassword = hex:list_to_hexstr(ucp_utils:to_ira(Password)),
    Body = #ucp60{
      oadc=Login,
      styp="1",
      pwd = IRAPassword,
      vers = "0100",
      opid = "39"},

    MessageLen = length(Login++IRAPassword) + ?HEADER_LEN + ?EMPTY_BODY_LEN_60,

    Header = #header{
      trn="00",
      len=ucp_utils:fill_with_zeros(MessageLen),
      o_r="O",
      ot="60"},

    UcpMessage = ucp_utils:compose_message(Header, Body),
    {ok, UcpMessage}.
