-module(gen_serverizer_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([get_bootstrap_modules/0, get_env/1, get_env/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    gen_serverizer_sup:start_link().

stop(_State) ->
    ok.

get_env(VarName) ->
    get_env(VarName,fun()-> throw({var_not_configured,VarName}) end).

get_env(VarName, Fallback) ->
    case application:get_env(gen_serverizer,VarName) of
        {ok, Value} -> Value;
        undefined ->
            if is_function(Fallback) -> Fallback();
               true -> Fallback
            end
    end.

get_bootstrap_modules() ->
    get_env(bootstrap_modules, []).
