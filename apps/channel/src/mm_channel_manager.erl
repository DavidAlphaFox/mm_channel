-module(mm_channel_manager).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
		  supervisor,
		  channel,
		  store,
		  subscribers = []
		 }).

start_link(ChannelSup, Channel) ->
    gen_server:start_link(?MODULE, [ChannelSup, Channel], []).

init([ChannelSup, Channel]) ->
	{ok,Store} = mm_msg_store:start_link(Channel),
    {ok, #state{
            supervisor = ChannelSup,
            channel = Channel,
			store = Store}}.

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
    {noreply, State#state{subscribers = []}}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
	{noreply, State#state{ subscribers = proplists:delete(Ref, State#state.subscribers)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


sync_key_current(Store)->
	{ok,Size} = mm_msg_store:size(Store),
	Size.
	
pull(Store,SyncKey,CurrentKey)->
	{ok,Msgs} = mm_msg_store:range(Store,SyncKey,CurrentKey),
	Msgs.

push(Store,Msg)->
	{ok,Key} = mm_msg_store:write(Store,Msg),
	Key.

newer_messages(Store,SyncKey) ->
	CurrentKey = sync_key_current(Store),
	SyncSeq = erlang:binary_to_integer(SyncKey),
	CurrentSeq = erlang:binary_to_integer(CurrentKey),
	Messages = if
				   CurrentSeq == SyncSeq ->
					   [];
				   CurrentSeq < SyncSeq ->
					   [];
				   true->
					   pull(Store,SyncKey,CurrentKey)
			   end,
	{CurrentKey,Messages}.

pull_messages(SyncKey, Subscriber, State) ->
	{NewSyncKey,ReturnMessages} = newer_messages(State#state.store,SyncKey),
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
