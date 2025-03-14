%%%-------------------------------------------------------------------
%% @doc ws_client top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ws_client_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-export([
    start_ws_client/4,
    stop_ws_client/1
]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_ws_client(CallbackModule, Host, Port, WsPath) ->
    supervisor:start_child(?MODULE, [CallbackModule, Host, Port, WsPath]).

stop_ws_client(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    ClientChild = #{
        id => ws_client,
        start => {ws_client, start_link, []},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => [ws_client]
    },
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 10
    },
    ChildSpecs = [ClientChild],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
