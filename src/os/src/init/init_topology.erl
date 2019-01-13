-module(init_topology).
-export([start/0, requires/0]).

start() ->
	os_topology:start().

requires() -> [init_wm, init_sdict].
