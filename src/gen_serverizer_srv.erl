%%%-------------------------------------------------------------------
%%% @author Vyacheslav Vorobyov <vjache@gmail.com>
%%% @copyright (C) 2013, Vyacheslav Vorobyov
%%% @doc
%%%
%%% @end
%%% Created : 12 Feb 2013 by Vyacheslav Vorobyov <vjache@gmail.com>
%%%-------------------------------------------------------------------
-module(gen_serverizer_srv).

-behaviour(gen_server).

%% API
-export([start_link/1, 
	 start_link_from_module/4,
	 register_call/3, 
	 register_call/4,
	 register_module/2,
	 register_module/3]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-define(SERVER, ?MODULE). 
-define(MANDATORY, exit(mandatory)).

-record(state, {calls = dict:new(), call_name_tran_fun}).
-record(call_reg_info, 
	{call_name = ?MANDATORY, 
	 registrator = ?MANDATORY, 
	 serialized = true, 
	 supervised = false, 
	 func = ?MANDATORY}).

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
start_link(ServerName) ->
    gen_server:start_link(ServerName, ?MODULE, [], []).

start_link_from_module(ServerName, Module, CallNameTranFun, Opts) when is_function(CallNameTranFun, 1) ->
    {ok, _Pid} = Ret = gen_server:start_link(ServerName, ?MODULE, [CallNameTranFun], []),
    ok = register_module(ServerName, Module, Opts),
    Ret.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

register_call(ServerRef, CallName, Fun) ->
    register_call(ServerRef, CallName, Fun, []).

register_call(ServerRef, CallName, Fun, Opts) 
  when is_function(Fun, 1), 
       is_list(Opts) ->
    Registrator = self(),
    Supervised = lists:member(sup, Opts),
    Serialized = lists:member(ser, Opts),
    [] == (Opts -- [sup,ser]) orelse exit(badarg),
    ok = gen_server:call(
	   ServerRef,
	   {'$register_call$', 
	    Registrator, 
	    Supervised, 
	    Serialized,
	    CallName,
	    Fun}).

register_module(ServerRef, Module) ->
    register_module(ServerRef, Module, []).

register_module(ServerRef, Module, Opts) when is_atom(Module), is_list(Opts) ->
    Opts1 = Opts -- [no_module_prefix],
    IsModPrefixed = (Opts == Opts1),
    [begin
	 CallName = if IsModPrefixed -> {Module, Func, Arity}; true -> {Func, Arity} end,
	 register_call(
	   ServerRef, 
	   CallName, 
	   fun(Tuple) when tuple_size(Tuple) == (Arity+1) -> 
		   [_ | Args] = tuple_to_list(Tuple),
		   apply(Module, Func, Args)
	   end,
	   Opts1)
     end || {Func, Arity} <- Module:module_info(exports)],
    ok.

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
    {ok, #state{}};
init([CallNameTranFun]) when is_function(CallNameTranFun, 1) ->
    {ok, #state{call_name_tran_fun = CallNameTranFun}}.

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
handle_call({'$register_call$', 
	     RegistratorPid, 
	     Supervised, 
	     Serialized, 
	     CallName, 
	     Fun}, 
	    _From, #state{calls = Calls}=State) ->
    case dict:is_key(CallName, Calls) of
	true ->
	    {reply, {error, {already_registered, CallName}}};
	false ->
	    Calls1 = dict:store(
		       CallName, 
		       #call_reg_info{
			  call_name = CallName,
			  registrator = RegistratorPid, 
			  serialized = Serialized,
			  supervised = 
			      if Supervised ->
				      erlang:monitor(process, RegistratorPid);
				 true ->
				      false
			      end, 
			  func = Fun}, Calls),
	    {reply, ok, State#state{calls = Calls1} }
    end;
handle_call(Request, _From, #state{ calls = Calls, call_name_tran_fun = CallNameTranFun } = State) ->
    CallName = if CallNameTranFun == undefined -> 
		       if is_tuple(Request) ->
			       element(1, Request);
			  is_list(Request) ->
			       hd(Request);
			  is_atom(Request) ->
			       Request
		       end; 
		  true -> 
		       CallNameTranFun(Request) 
	       end,
    case dict:find(CallName, Calls) of
	{ok, #call_reg_info{func = Fun} } ->
	    {reply, try {ok, Fun(Request)}
		    catch _:Reason ->
			    {error, {Reason, erlang:get_stacktrace()} }
		    end, State };
	error -> 
	    {reply, {error, {unexpected_call, CallName} } , State}
    end.

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
handle_info({'DOWN', Ref, process, Pid, _Reason}, 
	    #state{ calls = Calls } = State) ->
    Calls1 = dict:fold(
	       fun(CallName, 
		   #call_reg_info{registrator = RegPid,
				  supervised = SupRef}, 
		   CallsIn) when RegPid == Pid, SupRef == Ref -> 
		       dict:erase(CallName, CallsIn);
		  (_, _, CallsIn) ->
		       CallsIn
	       end, [],
	       Calls),
    {noreply, State#state{ calls = Calls1 } };
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
