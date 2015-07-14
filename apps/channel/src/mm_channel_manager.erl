-module(mm_channel_manager).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
		  supervisor,
		  channel,
		  subscribers = []
		 }).

start_link(ChannelSup, Channel) ->
    gen_server:start_link(?MODULE, [ChannelSup, Channel], []).

init([ChannelSup, Channel]) ->
    {ok, #state{
            supervisor = ChannelSup,
            channel = Channel}}.

handle_call(_From, _, State) ->
    {noreply, State}.


handle_cast({subscribe, SyncKey, Subscriber}, State) ->
    {NewSyncKey,NewSubscribers} = pull_messages(SyncKey, Subscriber, State),
    {noreply, State#state{ subscribers = NewSubscribers}};

handle_cast({push, Msg}, State) ->
	SyncKey = push(State#state.channel,Msg),
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

sync_key_seq(A)->
	[_,SeqBin] = binary:split(A,<<":">>),
	erlang:binary_to_integer(SeqBin).

sync_key_compare(A,B)->
	SeqA = sync_key_seq(A),
	SeqB = sync_key_seq(B),
	if
		SeqA > SeqB ->
			1;
		SeqA < SeqB ->
			-1;
		true ->
			0
	end.

sync_key_seq_current(Channel)->
	Fun = fun(C) ->
				  UUID = Channel ++ ":uuid",
				  {ok,R1} = eredis:q(C,[ "GET",UUID ]),
				  erlang:binary_to_integer(R1)
		  end,
	poolboy:transaction(channels_pool,Fun).

pull(Channel,SyncSeq,CurrentSeq)->
	Fun = fun(C)->
				  {ok,R1} = eredis:q(C,["ZRANGEBYSCORE",Channel,SyncSeq,CurrentSeq]),
				  R1
		  end,
	poolboy:transaction(channels_pool,Fun).

push(Channel,Msg)->
	Fun = fun(C)->
				  UUID = Channel ++ ":uuid",
				  {ok,R1} = eredis:q(C,["INCR",UUID ]),
				  {ok,_R2} = eredis:q(C,["ZADD",Channel,R1,Msg]),
				  NewSyncKey = Channel ++ ":" ++ erlang:binary_to_list(R1),
				  erlang:list_to_binary(NewSyncKey)
		  end,
	poolboy:transaction(channels_pool,Fun).

messages_newer_than_sync_key(Channel,SyncKey) ->
	SyncSeq = sync_key_seq(SyncKey),
	CurrentSeq = sync_key_seq_current(Channel),
	Messages = if
				   CurrentSeq == SyncSeq ->
					   [];
				   CurrentSeq < SyncKey ->
					   [];
				   true->
					   pull(Channel,SyncSeq,CurrentSeq)
			   end,
	NewSyncKey = Channel ++ ":" ++ erlang:integer_to_list(CurrentSeq),
	{erlang:list_to_binary(NewSyncKey),Messages}.

pull_messages(SyncKey, Subscriber, State) ->
	{NewSyncKey,ReturnMessages} = messages_newer_than_sync_key(State#state.channel,SyncKey),
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
