%%%-------------------------------------------------------------------
%%% @author Rafał Gałczyński <rafal.galczynski@gmail.com>
%%% @copyright (C) 2011, Rafał Gałczyński
%%% @doc
%%%
%%% @end
%%% Created :  7 Apr 2011 by Rafał Gałczyński <rafal.galczynski@gmail.com>
%%%-------------------------------------------------------------------
-module(smsc_pool).

-behaviour(gen_server).

-compile([debug_info]).

-include("../../include/logger.hrl").
-include("../../include/smsc_retry.hrl").
-include("../../include/utils.hrl").

%% API
-export([
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, send_message/4]).

-define(SERVER, ?MODULE).
-define(POOL_NAME, "smsc_pool").
-record(state, {
          smsc_config %% actual smsc configuration
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    pg2:create(?POOL_NAME),
    {ok, SMSCList} = load_config(),
    lists:foreach(
      fun({UName, Host, Port, Login, Password, State}) ->
              case State of
                  up ->
                      start_connection(Host, Port, Login, Password, UName);
                  Else ->
                      ?APP_DEBUG("Not starting connection, state is ~p", [Else])
              end
      end, SMSCList),
    State = #state{smsc_config = SMSCList},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({send_sms, {Sender, Receiver, Message, ReqId}}, _From, State) ->
    ?APP_DEBUG("Received send_sms call"),
    Reply = send_message(Sender, Receiver, Message, ReqId),
    {reply, Reply, State};

handle_call(do_rebuild_pool, _From, State) ->
    ?APP_DEBUG("Received rebuild call"),
    %% dump of actual configuration
    fs_utils:dump("smsc.conf",State#state.smsc_config),
    %% loading new config
    {ok, SMSCConfig} = load_config(),
    NewState = State#state{smsc_config = SMSCConfig},
    Reply = rebuild_pool(SMSCConfig),
    {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
    ?APP_DEBUG("Received unknow call"),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({send_sms, {Sender, Receiver, Message, ReqId}}, State) ->
    ?APP_DEBUG("Received send_sms cast"),
    send_message(Sender, Receiver, Message, ReqId),
    {noreply, State};

handle_cast(_Msg, State) ->
    ?APP_DEBUG("Received unknow cast"),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function used to sends sms messages
%% @spec do_request({Fun, [{Sender, Receiver, Message, ReqId}]}) ->
%%                                                     ok | Error
%%
%% @end
%%--------------------------------------------------------------------

do_request({_F, Args}, 0) ->
    ?APP_ERROR("error sending message [~p]",[Args]),
    {Sender, Receiver, Message, ReqId} = Args,
    Rec = #smsc_retry{ sender =Sender,
                       receiver = Receiver,
                       message = Message,
                       reqid = ReqId},
    %% tutja sprawdzic
    mnesia:dirty_write(Rec),
    ?APP_ERROR("Message stored in mnesia [~p]",[Args]);


do_request({F, {Receiver, Sender, Message, ReqId}}, RetryCount) ->
    case pg2:get_closest_pid(?POOL_NAME) of
        Pid when is_pid(Pid) ->
            case gen_server:call(Pid, {F, {Sender, Receiver, Message, ReqId}}) of
                ok ->
                    ?APP_DEBUG("sms sended to ~p", [Receiver]),
                    eventmon:notify({smsc_sent, ok });
                error ->
                    ?APP_ERROR("error sending trying via different connection"),
                    eventmon:notify({smsc_sent, nok }),
                    ok = do_request({F, {Sender, Receiver, Message, ReqId}}, RetryCount-1);
                Opps ->
                    eventmon:notify({smsc_sent, nok }),
                    ?APP_ERROR("Opps -> ~p",[Opps])
            end,
            ok;
        Err ->
            ?APP_DEBUG("Error getting connection from pool -> ~p",[Err]),
            Err
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function used to sends sms messages
%% @spec send_message(Sender, Receiver, Message, ReqId) -> {ok} | Error
%%
%% @end
%%--------------------------------------------------------------------

send_message(Sender, Receiver, Message, ReqId) ->
    PoolSize = length(pg2:get_members(?POOL_NAME)),
    do_request({send_message,
                {Sender, Receiver, Message, ReqId}}, PoolSize).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function loads new config from file
%% @spec load_config(filename) -> {ok, SMSConfig } | {error, Reason }
%%
%% @end
%%--------------------------------------------------------------------

load_config(Filename) ->
    ?SYS_INFO("Loading smscconfig: ~s", [Filename]),
    case file:consult(?PRIV(Filename)) of
        {ok, SMSCConf} ->
            ValidConfig = lists:map(
                            fun(ConfLine) ->
                                    {ok, ValidLine} = validate(ConfLine),
                                    ValidLine
                            end, SMSCConf),

            {ok, ValidConfig};
        {error, Reason} ->
            ?SYS_FATAL("Invalid ~s", [Filename]),
            {error, {smsc_config_corrupted, Filename, Reason}}
    end.

load_config() ->
    {ok, SMSConfig} = load_config("smsc.conf"),
    {ok, SMSConfig}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function validates configuration file
%% @spec validate({UName, Host, Port, Login, Password, up}) ->
%%    {ok, {UName, Host, Port, Login, Password, up}} |
%%    {ok, {UName, Host, Port, Login, Password, down}} |
%%    error
%%
%% @end
%%--------------------------------------------------------------------

validate({UName, Host, Port, Login, Password, up}) when
      is_atom(UName),
      is_list(Host),
      is_integer(Port),
      is_list(Login),
      is_list(Password) ->
    {ok, {UName, Host, Port, Login, Password, up}};

validate({UName, Host, Port, Login, Password, down}) when
      is_atom(UName),
      is_list(Host),
      is_integer(Port),
      is_list(Login),
      is_list(Password) ->
    {ok, {UName, Host, Port, Login, Password, down}};

validate(Oops) ->
    ?SYS_FATAL("Cannot parse smsc_config ~p", [Oops]),
    error.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function rebuilds actual pool of connections using actual configuration
%%
%% @end
%%--------------------------------------------------------------------

rebuild_pool(NewConfig) ->
    ConnMeta = lists:foldl(
                 fun(Elem, Acc) ->
                         Name = gen_server:call(Elem,get_name),
                         [Name|Acc]
                 end,
                 [],
                 pg2:get_members(?POOL_NAME)),
    lists:foreach(
      fun(ConfLine) ->
              {UName, _Host, _Port, _Login, _Password, Status} = ConfLine,
              NewConnectionInConfig = lists:member(UName, ConnMeta),
              handle_connection(ConfLine, Status, NewConnectionInConfig)
      end, NewConfig),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function starts new smsc connection and adds it into pool
%%
%% @spec start_connection(Host, Port, Login, Password, UName) ->
%%                                         ok | {error, Reason }
%% @end
%%--------------------------------------------------------------------

start_connection(Host, Port, Login, Password, UName) ->
    case smsc_connection:start_link(
           Host, Port, Login, Password, UName) of
        {ok, Pid} ->
            case pg2:join(?POOL_NAME, Pid) of
                ok ->
                    ?APP_DEBUG("smsc ~p connection adding to pool",[UName]),
                    ok;
                {error, Reason} ->
                    ?APP_DEBUG("pool error ~p", [Reason]),
                    {error, Reason}
            end;
        Error ->
            ?APP_ERROR("smsc ~p connection error ~p",[UName,Error]),
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function witch decide what to do with actual connection in case
%% of actual configuration
%%
%% @spec handle_connection(ConfigurationLine, State, IsExisting) -> ok
%% @end
%%--------------------------------------------------------------------

%% new config defines that connection shoulbe up, and we did not found
%% it in actual pool
handle_connection(ConfLine, up, false) ->
    {UName, Host, Port, Login, Password, _State} = ConfLine,
    ?APP_DEBUG("Found new conn [~p]in config, raising connection",[UName]),
    start_connection(Host, Port, Login, Password, UName),
    ok;

%% new config defines that connection shoulbe up, and we found it
%% in actual pool
handle_connection(ConfLine, up, true) ->
    %% check if actual config is the same, if not, kill connection
    %% and set the new one
    {UName, Host, Port, Login, Password, _State} = ConfLine,
    {OldUName, OldHost, OldPort, OldLogin, OldPassword} =
        gen_server:call({global,UName}, get_actual_config),
    case {Host, Port, Login, Password} =:= {OldHost, OldPort, OldLogin, OldPassword} of
        false ->
            ?APP_DEBUG("Config for [~p] changed, restarting connection",[OldUName]),
            gen_server:call({global, OldUName}, stop),
            start_connection(Host, Port, Login, Password, UName);
        true ->
            ?APP_DEBUG("Config for [~p] not changed",[OldUName])
    end,

    ok;

%% new config defines that connection shoulbe down, and we found it
%% in actual pool
handle_connection(ConfLine, down, true) ->
    {UName, _Host, _Port, _Login, _Password, _State} = ConfLine,
    ?APP_DEBUG("Config says to close conn [~p], closing connection",[UName]),
    gen_server:call({global, UName}, terminate);

%% new config defines that connection shoulbe down, and we did not
%% found it in actual pool
handle_connection(_ConfLine, down, false) ->
    %% so we are doin nothing
    ok.

