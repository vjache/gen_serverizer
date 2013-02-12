
-module(gen_serverizer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, 
	 start_child/1,
	 start_child_from_module/4]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Mod, Type, StartFunc, Args), {I, {Mod, StartFunc, Args}, permanent, 5000, Type, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(ServerName) ->
    Mod = gen_serverizer_srv,
    supervisor:start_child(
      ?MODULE, 
      ?CHILD({Mod, ServerName}, 
	     Mod, 
	     worker, 
	     start_link,
	     [ServerName])).

start_child_from_module(ServerName, Module, CallNameTranFun, Opts) ->
    Mod = gen_serverizer_srv,
    supervisor:start_child(
      ?MODULE, 
      ?CHILD({Mod, ServerName}, 
	     Mod, 
	     worker, 
	     start_link_from_module,
	     [ServerName, Module, CallNameTranFun, Opts])).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

