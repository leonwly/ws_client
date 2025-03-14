ws_client
=====

Websocket client based on library gun(https://github.com/ninenines/gun)

Build
-----

    $ rebar3 compile

Run
-----

    Host = "localhost",
	Port = 8091,
	WsPath = "/web_cmd",
    CallbackModule = a_module_to_handle_ws_msg,
	{ok, ClientPid} = ws_client_sup:start_ws_client(CallbackModule, Host, Port, WsPath).

Example
-----
    ws_client_example_mod.erl as simple example callback module
    ws_client_example.erl as gen_server example callback module
