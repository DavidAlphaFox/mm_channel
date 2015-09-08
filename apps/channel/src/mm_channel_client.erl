%%%-------------------------------------------------------------------
%%% @author davidalphafox <>
%%% @copyright (C) 2015, davidalphafox
%%% @doc
%%%
%%% @end
%%% Created : 14 Jul 2015 by davidalphafox <>
%%%-------------------------------------------------------------------
-module(mm_channel_client).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).
-export([new_channel/2,flush_channel/3,flush_channel/2]).

-define(SERVER, ?MODULE).

-record(state, {
		  channel,
		  sync_key
		 }).

%%%===================================================================
%%% API
%%%===================================================================

new_channel(ChannelName,Count)->
	SyncKey = <<"0"/utf8>>,
	Self = self(),
	{ok,Channel} = mm_channel_manager:start_link(Self,ChannelName),
	new_channel_client(Channel,SyncKey,Count),
	Channel.
new_channel_client(_Channel,_SyncKey,0)->
	ok;
new_channel_client(Channel,SyncKey,Count)->
	mm_channel_client:start_link(Channel,SyncKey),
	new_channel_client(Channel,SyncKey,Count-1).
flush_channel(_Channel,_Count,0)->
	ok;
flush_channel(Channel,Count,ProcessCount)->
	spawn(mm_channel_client, flush_channel, [Channel,Count]),
	flush_channel(Channel,Count,ProcessCount-1).
flush_channel(_Channel,0)->
	ok;
flush_channel(Channel,Count)->	
	gen_server:cast(Channel,{push,<<"a">>}),
	flush_channel(Channel,Count -1).				  

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Channel,SyncKey) ->

	gen_server:start_link( ?MODULE, [Channel,SyncKey], []).

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
init([Channel,SyncKey]) ->
	Self = self(),
	gen_server:cast(Channel,{subscribe,SyncKey,Self}),
	{ok, #state{
			channel = Channel,
			sync_key = SyncKey
		   }}.

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
handle_info({Channel,NewSyncKey,Messages},#state{channel = Channel} = State )->
	Self = self(),
	%Fun = fun(I)->
	%			  io:format("PID:~p Message:~p~n",[Self,I])
	%	  end,
	%lists:foreach(Fun,Messages),
	%io:format("PID:~p,NewSyncKey:~p~n",[Self,NewSyncKey]),
	%gen_server:cast(Channel,{subscribe,NewSyncKey,Self}),
	{noreply,State#state{sync_key = NewSyncKey}}; 
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
