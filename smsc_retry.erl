-module(smsc_retry).
-author('rafal.galczynski@jtendo.com').

-include_lib("stdlib/include/qlc.hrl").
-include("../../include/logger.hrl").
-include("../../include/smsc_retry.hrl").
-compile([debug_info]).

-define(SMSC_RETRY_VALIDITY, mmsp_conf:get_env(smsc_retry_validity, 1000)).
-define(SMSC_RETRY_WAIT, mmsp_conf:get_env(smsc_retry_wait, 1000)).
-define(SMSC_RETRY_TABLE,
        {smsc_retry, [
                      {disc_copies, [node()]},
                      {index, [first_fail]},
                      {attributes, record_info(fields,smsc_retry)}
                     ]}).

-export([start_retry/0, init_once/0, select_all/0, get_time_delta/1]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initialize function from smsc retry process
%%
%% @spec init_once()
%% @end
%%--------------------------------------------------------------------

init_once() ->
    ?APP_DEBUG("Initializing smsc_retry table"),
    mnesia:create_schema([node()]),
    {TableName, Opts} = ?SMSC_RETRY_TABLE,
    mnesia:create_table(TableName, Opts),
    ?APP_DEBUG("Waiting for smsc_retry table, ~p ~p",[TableName, Opts]),
    mnesia:wait_for_tables([smsc_retry], 20000),
    ?APP_DEBUG("SMSCRetry table found.").


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop for starting resending stored sms messages
%%
%% @spec start_retry()
%% @end
%%--------------------------------------------------------------------

start_retry() ->
    receive
    after ?SMSC_RETRY_WAIT
              ->
            {atomic, ToSend} = select_all(),
            ?APP_DEBUG("Trying to resend messages, ~p waiting", [length(ToSend)]),
            lists:foreach(
              fun(Elem) -> resend(Elem) end,
                           ToSend)
    end,
    start_retry().


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function used for sending stored messages as records
%%
%% @spec resend(#smsc_retry)
%% @end
%%--------------------------------------------------------------------

resend(Rec) ->
    ?APP_DEBUG("Trying to resend message ~p",[Rec]),
    {smsc_retry, Sender, Receiver, Message, FirstTry} = Rec,
    case compare(get_time_delta(FirstTry), ?SMSC_RETRY_VALIDITY) of
        greater ->
            ?APP_DEBUG("Message validity time expired, deleting ~p",[Rec]),
            mnesia:dirty_delete_object(Rec),
            ok;
        _Else ->
            ?APP_DEBUG("Sending message ~p",[Rec]),
            case smsc_pool:send_message(Sender, Receiver, Message) of
                ok ->
                    ?APP_DEBUG("Message sended, deleting ~p",[Rec]),
                    mnesia:dirty_delete_object(Rec),
                    ok;
                _NotOk ->
                    ?APP_ERROR("Errro resending message, will try later")
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function used for getting all waiting messages from mnesia db
%%
%% @spec select_all()
%% @end
%%--------------------------------------------------------------------

select_all() ->
    %% mnesia:dirty_read(;;adasd)
    mnesia:transaction(
      fun() ->
              qlc:eval( qlc:q(
                          [ X || X <- mnesia:table(smsc_retry) ]
                         ))
      end ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function calculating TimeDelta between last send and now
%%
%% @spec select_all()
%% @end
%%--------------------------------------------------------------------

get_time_delta(FirstTry) ->
    First = calendar:datetime_to_gregorian_seconds(FirstTry),
    Now = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
    Now-First.

compare(X, Y) ->
    if X>Y -> greater;
       X==Y -> equal;
       true -> less
    end.
