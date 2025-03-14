%%%-------------------------------------------------------------------
%%% @author wang
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%		This is an example of a websocket client callback module
%%% @end
%%%-------------------------------------------------------------------
-module(ws_client_example).

-behaviour(gen_server).
-behavior(ws_client).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).
-export([ws_handle_up/1, ws_handle_upgrade/2, ws_handle_down/2, ws_handle_error/3,
	ws_handle_frame/3]).

-export([
	send/1
]).

-define(SERVER, ?MODULE).

-record(ws_client_example_state, {
	client_pid	:: pid(),
	gun_pid		:: pid(),
	ws_ref		:: reference(),
	sent_count	:: integer(),		% 发送计数
	recv_count	:: integer()		% 接受计数
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send(Message) ->
	gen_server:cast(?MODULE, {send, Message}).

init([]) ->
	Host = "localhost",
	Port = 8091,
	WsPath = "/web_cmd",
	{ok, ClientPid} = ws_client_sup:start_ws_client(?MODULE, Host, Port, WsPath),
	{ok, #ws_client_example_state{client_pid = ClientPid, sent_count = 0, recv_count = 0}}.

handle_call(_Request, _From, State = #ws_client_example_state{}) ->
	{reply, ok, State}.

handle_cast({set_gun_pid, GunPid}, State) ->
	{noreply, State#ws_client_example_state{gun_pid = GunPid}};

handle_cast({set_ws_ref, GunPid, WsRef}, #ws_client_example_state{gun_pid = GunPid}=State) ->
	{noreply, State#ws_client_example_state{ws_ref = WsRef}};

handle_cast({send, Message}, #ws_client_example_state{client_pid = ClientPid, sent_count = SentCount}=State) ->
	ws_client:send(ClientPid, {text, Message}),
	{noreply, State#ws_client_example_state{sent_count = SentCount + 1}};

handle_cast({ws_data, GunPid, WsRef, Frame}, #ws_client_example_state{gun_pid = GunPid, ws_ref = WsRef, recv_count = RecvCount}=State) ->
	io:format("~p~n", [{?MODULE, ?LINE, Frame}]),
	{noreply, State#ws_client_example_state{recv_count = RecvCount + 1}};

handle_cast(_Request, State = #ws_client_example_state{}) ->
	{noreply, State}.

handle_info(_Info, State = #ws_client_example_state{}) ->
	{noreply, State}.

terminate(_Reason, _State = #ws_client_example_state{client_pid = ClientPid}) ->
	ws_client_sup:stop_ws_client(ClientPid),
	ok.

code_change(_OldVsn, State = #ws_client_example_state{}, _Extra) ->
	{ok, State}.

ws_handle_up(GunPid) ->
	io:format("========+++++========~p~n", [{?MODULE, ?LINE, GunPid}]),
	gen_server:cast(?MODULE, {set_gun_pid, GunPid}),
	ok.

ws_handle_upgrade(GunPid, WsRef) ->
	io:format("========+++++========~p~n", [{?MODULE, ?LINE, GunPid, WsRef}]),
	gen_server:cast(?MODULE, {set_ws_ref, GunPid, WsRef}),
	ok.

ws_handle_down(GunPid, Reason) ->
	io:format("========+++++========~p~n", [{?MODULE, ?LINE, GunPid, Reason}]),
	ok.

ws_handle_frame(GunPid, WsRef, Frame) ->
	io:format("========+++++========~p~n", [{?MODULE, ?LINE, GunPid, WsRef, Frame}]),
	gen_server:cast(?MODULE, {ws_data, GunPid, WsRef, Frame}),
	ok.

ws_handle_error(GunPid, WsRef, Reason) ->
	io:format("========+++++========~p~n", [{?MODULE, ?LINE, GunPid, WsRef, Reason}]),
	ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
