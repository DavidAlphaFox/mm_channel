-module(mm_channel_config).
-export([pools/0]).

pools()->
	R = application:get_env(mm_channel,pools),
	case R of
		{ok,Pools}->
			Pools;
		_ ->
			undefined
	end.
