%%%-------------------------------------------------------------------
%%% @author Rafał Gałczyński <>
%%% @copyright (C) 2011, Rafał Gałczyński
%%% @doc
%%%
%%% @end
%%% Created : 16 Mar 2011 by Rafał Gałczyński <>
%%%-------------------------------------------------------------------
-module(smsc_connection).

-behaviour(gen_server).

-include("../../include/ucp/ucp_syntax.hrl").
-include("../../include/logger.hrl").

-compile([debug_info]).
%% API
-export([start_link/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-define(SMSC_REPLY_TIMEOUT, 2000).
-define(SMSC_KEEPALIVE_TIME, 92000).
-define(SMSC_RECONNECT_TIME, 2000).

-record(state, {
          socket,   %% smsc socket
          addr,     %% smsc address
          port,     %% smsc port
          login,    %% smsc login
          pass,     %% smsc password
          state,    %% smsc socket state
          last_usage, %% timestamp of last socket usage
          unique_name, %% unique name of connection
          ucp_ipport, %% unique name of connection
          seq         %% last used seq
         }).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Host, Port, Login, Password, UName) ->
    gen_server:start_link({global, UName},
                          ?MODULE, [Host, Port, Login, Password, UName], []).

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
init([Address, Port, Login, Password, UName]) ->
    process_flag(trap_exit, true),
    UcpIP = lists:foldr(
              fun(X, Acc) ->
                      [ucp_utils:fill_with_zeros(X,3)|Acc]
              end,
              [],
              string:tokens(Address,".")),
    State = #state{ addr=Address, port=Port, pass=Password,
                    login=Login, state=disconnected,
                    last_usage=erlang:now(), unique_name = UName,
                  ucp_ipport = lists:flatten(UcpIP) ++ integer_to_list(Port),
                  seq = "00"},
    case smsc_connect(State) of
        {ok, NState, _} ->
            spawn_link(fun() -> keepalive(NState) end),
            eventmon:notify({smsc_connection, ok }),
            {ok, NState};
        {error, _NState, Reason} ->
            eventmon:notify({smsc_connection, nok }),
            ?APP_ERROR("Error connecting to smsc ~p, ~p",
                       [State#state.unique_name, Reason]),
            {stop, normal}
    end.
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

handle_call({send_message,{Sender, Receiver, Msg, ReqId}}, _From, State) ->
    Seq = get_next_seq(State#state.seq),
    {ok, UcpMessage} = ucp_messages:create_m51(Seq, Sender, Receiver, Msg),
    {_Reply, NState, _Reason} = send_message(State, UcpMessage, ReqId),
    {reply, ok, NState#state{seq=Seq}};

handle_call(terminate, _From, State) ->
    ?APP_DEBUG("SMSC ~p received terminate call", [State#state.unique_name]),
    {stop, disabled_in_configuration , State};

handle_call(get_name, _From, State) ->
    Reply = State#state.unique_name,
    {reply, Reply , State};

handle_call(stop, _From, State)->
    {stop, normal, stopped, State};

handle_call(get_actual_config, _From, State) ->
    Uname = State#state.unique_name,
    Host = State#state.addr,
    Port = State#state.port,
    Login = State#state.login,
    Pass = State#state.pass,
    Reply = {Uname, Host, Port, Login, Pass},
    {reply, Reply , State};

handle_call({state_update,NewState}, _From, _State) ->
    {reply, ok , NewState};

handle_call(_Request, _From, State) ->
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

handle_cast(_Msg, State) ->
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
    ?APP_DEBUG("~p terminating",[self()]),
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

update_state(NewState) ->
    ?APP_DEBUG("Updateing state, new state -> ~p", [NewState]),
    gen_server:call(
      {global, NewState#state.unique_name},
      {state_update, NewState}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% KeepAlive function, should by spawned
%%
%% @spec keepalive(State) -> exit() | keepalive(State)
%% @end
%%--------------------------------------------------------------------

keepalive(State) ->
    receive
    after ?SMSC_KEEPALIVE_TIME ->
            Seq = get_next_seq(State#state.seq),
            {ok, UcpMessage} =
                ucp_messages:create_m31(Seq, State#state.ucp_ipport),
            case send_message(State, UcpMessage, keepalive) of
                {ok, NState, _ErrMsg} ->
                    keepalive(NState#state{seq = Seq});
                {error, NState, ErrMsg} ->
                    eventmon:notify({smsc_connection, nok }),
                    ?APP_ERROR("SMSC keepalive problem ~p, reconnecting",
                               [ErrMsg]),
                    {ok, RState} = smsc_reconnect(NState),
                    update_state(RState),
                    keepalive(RState)
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function try to reconnect to smsc
%%
%% @spec smsc_reconnect(State) -> {ok, State} |
%% @end
%%--------------------------------------------------------------------

smsc_reconnect(State) ->
    case smsc_connect(State) of
        {ok, NState} ->
            ?APP_DEBUG("SMSC Reconnection OK"),
            {ok, NState};
        {error, NState} ->
            ?APP_ERROR("SMSC Reconnection NOK, sleep and reconnect"),
            sleep(?SMSC_RECONNECT_TIME),
            smsc_reconnect(NState)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function try to connect and login  to smsc
%%
%% @spec smsc_connect(State) -> {ok, State, ErrMsg} | {error, State, ErrMsg}
%% @end
%%--------------------------------------------------------------------

smsc_connect(State) ->
    case gen_tcp:connect(
           State#state.addr,
           State#state.port,
           ?TCP_OPTIONS) of
        {ok, Socket} ->
            NewState = State#state{socket=Socket, state=connected},
            eventmon:notify({smsc_connection, ok}),
            smsc_login(
              NewState#state.login,
              NewState#state.pass,
              NewState);
        {error,Reason} ->
            NewState = State#state{state=Reason, socket=undefined},
            eventmon:notify({smsc_connection, nok }),
            ?APP_ERROR("Error connecting to SMSC -> ~p", [NewState]),
            {error, NewState, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function try to login to smsc
%%
%% @spec smsc_login(State) -> {ok, State, ErrMsg} | {error, State, ErrMsg}
%% @end
%%--------------------------------------------------------------------

smsc_login(Login, Password, State) ->
    Seq = get_next_seq(State#state.seq),
    {ok, UcpMessage} = ucp_messages:create_m60(Seq, Login,Password),
    send_message(State#state{seq = Seq}, UcpMessage, login).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function for sending UCP messages via smsc connection
%%
%% @spec send_message(State, UCPMessage) -> {ok, State} | {error, State}
%% @end
%%--------------------------------------------------------------------

send_message(State, UcpMessage, ReqId) ->
    Socket = State#state.socket,
    UName = State#state.unique_name,
    %% UsageTimeDiff = timer:now_diff(State#state.last_usage, erlang:now()),
    %% TODO, decide if we need some sleep before send
    case Socket of
        undefined ->
            {error, State, socket_undefined };
        _ ->
            case gen_tcp:send(Socket, UcpMessage) of
                ok ->
                    case gen_tcp:recv(Socket, 0, ?SMSC_REPLY_TIMEOUT) of
                        {ok, Data} ->
                            {_Header, Body} = ucp_utils:unpackUCP(Data),
                            NewState = State#state{last_usage = erlang:now()},
                            {Type, Error} = ucp_utils:analyze_ucp_body(Body),
                            case Type =:= ack of
                                true ->
                                    ?APP_DEBUG("SMSC ~p [ req ~p] resp -> ack",
                                               [UName, ReqId]),
                                    {ok, NewState, ok};
                                false ->
                                    ?APP_DEBUG("SMSC ~p resp -> ~p ~p",
                                               [UName, Type, Error]),
                                    {error, NewState, Error}
                            end;

                        {error, Reason} ->
                            ?APP_ERROR("NO SMSC resp [ req ~p] -> ~p",
                                       [ReqId, Reason]),
                            NewState = State#state{last_usage = erlang:now()},
                            {error, NewState, Reason}
                    end;
                {error, Reason} ->
                    ?APP_ERROR("SMSC Error sending -> ~p ~p ~p",
                               [ReqId, Reason, State]),
                    {error, State, Reason}
            end
    end.


sleep(T) ->
    receive
    after
        T -> true
    end.

get_next_seq(Str) ->
    Int = list_to_integer(Str),
    case Int =:= 99 of
        true ->
            "00";
        false ->
            ucp_utils:fill_with_zeros(Int+1,2)
    end.


