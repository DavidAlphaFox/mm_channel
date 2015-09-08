%%%-------------------------------------------------------------------
%%% @author davidalphafox <>
%%% @copyright (C) 2015, davidalphafox
%%% @doc
%%%
%%% @end
%%% Created :  8 Sep 2015 by davidalphafox <>
%%%-------------------------------------------------------------------
-module(mm_msg_store).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-export([write/2,size/1,range/3]).

-define(SERVER, ?MODULE).

-record(state, {
		  db,
		  channel
		 }).

%%%===================================================================
%%% API
%%%===================================================================
write(Store,Msg)->
	gen_server:call(Store,{write,Msg}).
size(Store)->
	gen_server:call(Store,{size}).
range(Store,Start,End)->
	gen_server:call(Store,{range,Start,End}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Channel) ->
	gen_server:start_link(?MODULE, [Channel], []).

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
init([Channel]) ->
	{ok,DB} = eleveldb:open(erlang:binary_to_list(Channel), [{create_if_missing, true}]), 
	State = #state{db = DB,channel = Channel},
	{ok, State}.

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
handle_call({write,Msg},_From,#state{db = DB} = State)->
	{ok,ID} = new_id(DB),
	ok = eleveldb:put(DB,ID,Msg,[]),
	{reply,{ok,ID},State};
handle_call({size},_From,#state{db = DB} = State)->
	{ok,ID} = current_id(DB),
	{reply,{ok,ID},State};
handle_call({range,Start,End},_From,#state{db = DB} = State)->
	{ok,Msgs} = range_internal(DB,Start,End), 
	{reply,{ok,Msgs},State};
	
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
terminate(_Reason, #state{db = DB}) ->
	eleveldb:close(DB),
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
new_id(DB)->
	{ok,IDBin} = current_id(DB),
	ID = erlang:binary_to_integer(IDBin),
	NewID = erlang:integer_to_binary(ID + 1),
	ok = eleveldb:put(DB,<<"0">>,NewID,[]),
	{ok,NewID}.
	
current_id(DB)->
	case eleveldb:get(DB, <<"0">>, []) of
        {ok, Value} ->			
			{ok, Value};
		not_found ->
			{ok,<<"0">>};
        {error, Reason} ->
            {error,Reason}
	end.
range_internal(DB,Start,End)->
	S = erlang:binary_to_integer(Start),
	Msgs = range_loop(DB,S+1,End,[]),
	{ok,Msgs}.

range_loop(DB,S,End,L)->
	Key = erlang:integer_to_binary(S),
	{ok,Value} = eleveldb:get(DB,Key,[]),
	if
		End == Key->
			lists:reverse([{Key,Value}|L]);
		true ->
			range_loop(DB,S + 1,End,[{Key,Value}|L])
	end.
