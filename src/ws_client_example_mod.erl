%%%-------------------------------------------------------------------
%%% @author wang
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%		This is an example of a simple callback module for a websocket client
%%% @end
%%% Created : 12. 3月 2025 17:14
%%%-------------------------------------------------------------------
-module(ws_client_example_mod).
-author("wang").

%% API
-export([start/0, stop/1]).

-export([
	ws_handle_up/1,
	ws_handle_upgrade/2,
	ws_handle_down/2,
	ws_handle_frame/3,
	ws_handle_error/3
]).

-export([
	send/2
]).

start() ->
	Host = "localhost",
	Port = 8091,
	WsPath = "/web_cmd",
	{ok, Pid} = ws_client_sup:start_ws_client(?MODULE, Host, Port, WsPath),
	Pid.

stop(Pid) ->
	ws_client_sup:stop_ws_client(Pid).

send(Pid, Message) ->
	Frame = {text, Message},
	ws_client:send(Pid, Frame).

ws_handle_up(GunPid) ->
	io:format("========+++++========~p~n", [{?MODULE, ?LINE, GunPid}]),
	todo.

ws_handle_upgrade(GunPid, WsRef) ->
	io:format("========+++++========~p~n", [{?MODULE, ?LINE, GunPid, WsRef}]),
	todo.

ws_handle_down(GunPid, Reason) ->
	io:format("========+++++========~p~n", [{?MODULE, ?LINE, GunPid, Reason}]),
	todo.

ws_handle_frame(GunPid, WsRef, Frame) ->
	io:format("========+++++========~p~n", [{?MODULE, ?LINE, GunPid, WsRef, Frame}]),
	todo.

ws_handle_error(GunPid, WsRef, Reason) ->
	io:format("========+++++========~p~n", [{?MODULE, ?LINE, GunPid, WsRef, Reason}]),
	todo.
