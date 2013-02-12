%%%-------------------------------------------------------------------
%%% @author Vyacheslav Vorobyov <vvorobyov@hydra>
%%% @copyright (C) 2013, Vyacheslav Vorobyov
%%% @doc
%%%
%%% @end
%%% Created : 12 Feb 2013 by Vyacheslav Vorobyov <vvorobyov@hydra>
%%%-------------------------------------------------------------------
-module(gen_serverizer).

%% API
-export([create_global_server_from_module/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%  Starts a globaly registered 'gen_server' instance that exposes 
%%  all exported functions of module passed. The name of a 'gen_server' 
%%  started is like {global, Module}. All functions of a module are 
%%  callable through gen_server:call as {FuncAtom, Arg1, Arg2, ... , ArgN}.
%% @example
%%   1> gen_serverizer:create_global_server_from_module(lists).
%%   {global,lists}
%%   2> gen_server:call({global, lists}, {max, [1,2,3,4,5,6]}).
%%   {ok, 6}
%% @end
%%--------------------------------------------------------------------
create_global_server_from_module(Module) ->
    ServerRef = {global, Module},
    {ok, _Pid} = gen_serverizer_sup:start_child_from_module(
		   ServerRef, 
		   Module,
		   fun (Request) when is_atom(Request) ->
			   {Request, 0};
		       (Request) when is_atom(element(1, Request)) ->
			   FuncName = element(1, Request),
			   {FuncName, tuple_size(Request) - 1};
		      (Request) when tuple_size(element(1, Request)) == 2,
				     is_atom(element(1, element(1, Request))),
				     is_integer(element(2, element(1, Request))) ->
			   element(1, Request)
		   end, 
		   [no_module_prefix]),
    ServerRef.
%%%===================================================================
%%% Internal functions
%%%===================================================================
