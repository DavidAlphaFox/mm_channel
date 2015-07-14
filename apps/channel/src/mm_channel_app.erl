-module(mm_channel_app).
-behaviour(application).

-export([start/2, stop/1]).



start(_StartType, _StartArgs) ->
	lager:start(),
	ok = application:ensure_started(inets),
	ok = application:ensure_started(crypto),
	mm_channel_sup:start_link().

stop(_State) ->
	lager:stop(),
	ok.



