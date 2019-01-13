-module(init_timecheck).
-export([start/0, requires/0]).
-export([server/0]).

start() ->
	spawn(fun server/0).


server() ->
	io:format("TIMECHECK: ~p.~n", [os_system_info:get_time()]),
	os_util:wait_ms(2500),
	server().


requires() -> [init_wm, init_time].
