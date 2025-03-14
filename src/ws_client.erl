%%%-------------------------------------------------------------------
%%% @author wang
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ws_client).

-behaviour(gen_server).

-export([start_link/4, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-export([
	send/2
]).

%% Common handler callbacks

-callback ws_handle_up(GunPid :: pid()) -> any().

-callback ws_handle_upgrade(GunPid :: pid(), WsRef :: gun:stream_ref()) -> any().

-callback ws_handle_down(GunPid :: pid(), WsRef :: gun:stream_ref()) -> any().

-callback ws_handle_error(GunPid :: pid(), WsRef :: gun:stream_ref(), Reason :: any()) -> any().

-callback ws_handle_frame(GunPid :: pid(), WsRef :: gun:stream_ref(), Frame) -> any()
	when Frame :: close | ping | pong
				| {text | binary | close, binary()}
				| {close, non_neg_integer(), binary()}
				| {ping | pong, binary()}.

-record(ws_client_state, {
	callback_module :: atom(),
	gun_pid 		:: pid(),
	ws_ref			:: reference(),
	ws_path			:: iolist()
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(CallbackModule, Host, Port, WsPath) ->
	gen_server:start_link(?MODULE, [CallbackModule, Host, Port, WsPath], []).

stop(ServerPid) ->
	gen_server:stop(ServerPid, normal, 5000).

send(ServerPid, Frames) ->
	gen_server:cast(ServerPid, {send, Frames}).

init([CallbackModule, Host, Port, WsPath]) ->
	{ok, GunPid} = gun:open(Host, Port,  #{protocols => [http]}),
	{ok, #ws_client_state{callback_module = CallbackModule, gun_pid = GunPid, ws_path = WsPath}}.

handle_call(_Request, _From, State = #ws_client_state{}) ->
	{reply, ok, State}.

handle_cast({send, Frames}, State = #ws_client_state{ws_ref = Ref}) ->
	gun:ws_send(State#ws_client_state.gun_pid, Ref, Frames),
	{noreply, State};

handle_cast(_Request, State = #ws_client_state{}) ->
	{noreply, State}.

handle_info({gun_up, NewGunPid, http}, #ws_client_state{ws_path = WsPath, callback_module = CallbackModule}=State) ->
	NewWsRef = gun:ws_upgrade(NewGunPid, WsPath),
	callback_apply(CallbackModule, ws_handle_up, [NewGunPid]),
	{noreply, State#ws_client_state{gun_pid = NewGunPid, ws_ref = NewWsRef}};

handle_info({gun_upgrade, GunPid, Ref, [<<"websocket">>], Headers}, #ws_client_state{ws_ref = Ref, callback_module = CallbackModule}=State) ->
	callback_apply(CallbackModule, ws_handle_upgrade, [GunPid, Ref]),
	{noreply, State#ws_client_state{ws_ref = Ref}};

handle_info({gun_down, GunPid, Protocol, Reason, KilledStreams}, #ws_client_state{callback_module = CallbackModule}=State) ->
	callback_apply(CallbackModule, ws_handle_down, [GunPid, Reason]),
	{stop, normal, State};

handle_info({gun_ws, GunPid, Ref, Frame}, #ws_client_state{callback_module = CallbackModule}=State) ->
	callback_apply(CallbackModule, ws_handle_frame, [GunPid, Ref, Frame]),
	{noreply, State};

handle_info({gun_error, GunPid, Ref, Reason}, #ws_client_state{callback_module = CallbackModule}=State) ->
	callback_apply(CallbackModule, ws_handle_error, [GunPid, Ref, Reason]),
	{stop, normal, State};

handle_info(_Info, State = #ws_client_state{}) ->
	{noreply, State}.

terminate(_Reason, _State = #ws_client_state{gun_pid = GunPid}) ->
	gun:close(GunPid),
	ok.

code_change(_OldVsn, State = #ws_client_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

callback_apply(CallbackModule, Fun, Args) ->
	try apply(CallbackModule, Fun, Args) of
		_ -> ok
	catch
		_:_ -> ok
	end.
