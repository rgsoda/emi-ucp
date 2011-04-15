-module(fakesmsc).
-include("../../include/ucp/ucp_syntax.hrl").
-include("../../include/logger.hrl").

-compile(export_all).
-define(TCP_OPTIONS, [
                      binary,
                      {packet, 0},
                      {active, false},
                      {send_timeout,5000},
                      {reuseaddr, true}
                     ]).
-define(ACK,<<16#02,"00/00039/R/51/A/012234:090996101010/68",16#03>>).
-define(NACK,<<16#02,"00/00034/R/60/N/02/Syntax Error/D9",16#03>>).

%% Call echo:listen(Port) to start the service.
listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).

%% Wait for incoming connections and spawn
%% the echo loop when we get one.
accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> loop(Socket) end),
    accept(LSocket).

%% Echo back whatever data we receive on Socket.
loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("->~p~n",[Data]),
            unpackUCP(Data),
            gen_tcp:send(Socket, ?NACK),
            loop(Socket);
        {error, closed} ->
            ok
    end.



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
        {TRN, LEN, OR, OT, ADC, OADC, AC, NRQ, NADC, NT, NPID,
         LRQ, LRAD, LPID, DD, DDT, VP, RPID, SCTS, DST, RSN, DSCTS,
         MT, NB, MSG, MMS, PR, DCS, MCLS, RPI, CPG, RPLY, OTOA, HPLMN,
         XSER, RES4, RES5, CRC} ->
            Header = #header{trn=TRN, len=LEN, o_r=OR, ot=OT},
            Body = #ucp5x{adc=ADC, oadc=OADC, ac=AC, nrq=NRQ, nadc=NADC,
                          nt=NT, npid=NPID, lrq=LRQ, lrad=LRAD, lpid=LPID,
                          dd=DD, ddt=DDT, vp=VP, rpid=RPID, scts=SCTS,
                          dst=DST, rsn=RSN, dscts=DSCTS, mt=MT, nb=NB,
                          msg=MSG, mms=MMS, pr=PR, dcs=DCS, mcls=MCLS,
                          rpi=RPI, cpg=CPG, rply=RPLY, otoa=OTOA, hplmn=HPLMN,
                          xser=XSER, res4=RES4, res5=RES5, crc=CRC},
            {Header, Body};
        {TRN, LEN, OR, OT, OADC, OTON, ONPI, STYP, PWD, NPWD, VERS,
         LADC, LTON, OPID, RES1, CRC} ->
            Header = #header{trn=TRN, len=LEN, o_r=OR, ot=OT},
            Body = #ucp60{oadc=OADC,oton=OTON, onpi=ONPI, styp=STYP,
                          pwd=PWD, npwd=NPWD, vers=VERS, ladc=LADC,
                          lton=LTON, opid=OPID, res1=RES1, crc=CRC},
            {Header, Body};
        {TRN, LEN, OR, OT, OADC, OTON, ONPI, STYP, PWD, NPWD, VERS,
         LADC, LTON, LNPI, RES1, RES2,CRC} ->
            Header = #header{trn=TRN, len=LEN, o_r=OR, ot=OT},
            Body = #ucp61{oadc=OADC,oton=OTON, onpi=ONPI, styp=STYP,
                          pwd=PWD, npwd=NPWD, vers=VERS, ladc=LADC,
                          lton=LTON, lnpi=LNPI, res1=RES1, res2=RES2, crc=CRC},
            {Header, Body};
        Other ->
            ?APP_DEBUG("~p ~p", ["Received WRONG MESSAGE",Other]),
            error
    end.

unpackGSM(GsmHexString) ->
    A = ucp_7bit:from_7bit(hex:bin_to_hexstr(GsmHexString)),
    io:format("~p ~n", [A]),
    B = utf:from_binary(hex:hexstr_to_bin(binary:bin_to_list(GsmHexString))),
    io:format("~p ~n", [B]).

