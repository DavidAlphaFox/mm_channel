-module(mm_channel_manager).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
		  supervisor,
		  channel,
		  store,
		  cache = [],
		  cache_count = 0,
		  max_cache_count = 100,
		  current_key,
		  subscribers = []
		 }).

start_link(ChannelSup, Channel) ->
    gen_server:start_link(?MODULE, [ChannelSup, Channel], []).

init([ChannelSup, Channel]) ->
	{ok,Store} = mm_msg_store:start_link(Channel),
	CurrentKey = current(Store),
    {ok, #state{
            supervisor = ChannelSup,
            channel = Channel,
			store = Store,
			current_key = CurrentKey}}.

handle_call(_From, _, State) ->
    {noreply, State}.


handle_cast({subscribe, SyncKey, Subscriber}, State) ->
    {NewSyncKey,NewSubscribers} = pull_messages(SyncKey, Subscriber, State),
    {noreply, State#state{ subscribers = NewSubscribers}};

handle_cast({push, Msg}, State) ->
	SyncKey = push(State#state.store,Msg),
    lists:foldr(fun({Ref, Sub}, _) ->
						Sub ! {self(), SyncKey, [Msg]},
						erlang:demonitor(Ref)						
        end,undefined, State#state.subscribers),
    NewState = cache(SyncKey,Msg,State#state{subscribers = []}),
    {noreply, NewState}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
	{noreply, State#state{ subscribers = proplists:delete(Ref, State#state.subscribers)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


current(Store)->
	{ok,Size} = mm_msg_store:size(Store),
	Size.

pull(Store,SyncKey,CurrentKey)->
	{ok,Msgs} = mm_msg_store:range(Store,SyncKey,CurrentKey),
	Msgs.

push(Store,Msg)->
	{ok,Key} = mm_msg_store:write(Store,Msg),
	Key.
cache(SyncKey,Msg,
	#state{cache = Cache,cache_count = Count,max_cache_count = Max} = State)->
	if 
		Count < Max ->
			State#state{cache = lists:append(Cache,[Msg]),
			cache_count = Count + 1,current_key = SyncKey};
		true ->
			[_H|T] = Cache,
			State#state{cache = lists:append(T,[Msg]),current_key = SyncKey}
	end.


pull_cache(SyncKey,State)->
	CurrentSeq = erlang:binary_to_integer(State#state.current_key),
	CachedSeq = CurrentSeq - State#state.cache_count,
	SyncSeq = erlang:binary_to_integer(SyncKey),
	Messages = if 
			(SyncSeq >= CachedSeq) and (SyncSeq < CurrentSeq) ->
				Diff = SyncSeq - CachedSeq,
				{_H,T} = lists:split(Diff,State#state.cache),
				T;
			SyncSeq < CachedSeq ->
				Older = pull(State#state.store,SyncKey,erlang:integer_to_binary(CachedSeq)),
				lists:append(Older,State#state.cache);
			true ->
				[]
		end,
	{State#state.current_key,Messages}.	
	
pull_messages(SyncKey, Subscriber, State) ->
	{NewSyncKey,ReturnMessages} = pull_cache(SyncKey,State),
    case  erlang:length(ReturnMessages) of
		0 ->
			{NewSyncKey,add_subscriber(Subscriber, State#state.subscribers)};
		_->
            Subscriber ! {self(), NewSyncKey, ReturnMessages},
            {NewSyncKey,State#state.subscribers}            
    end.

% Checks if the new subscriber pid already has a monitor
add_subscriber(NewSubscriber, Subscribers) ->
	case lists:keymember(NewSubscriber, 2, Subscribers) of
		true -> Subscribers;
		false -> [{erlang:monitor(process, NewSubscriber), NewSubscriber} | Subscribers]
	end.
