%%%-------------------------------------------------------------------
%%% @author David Alpha Fox <>
%%% @copyright (C) 2014, David Alpha Fox
%%% @doc
%%%
%%% @end
%%% Created :  2 Sep 2014 by David Alpha Fox <>
%%%-------------------------------------------------------------------
-module(mm_channel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,

	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

	Restart = permanent,
	Shutdown = 2000,
	Type = worker,
	Children = [],
	{ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
