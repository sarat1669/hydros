-module(init_ipc).
-export([start/0, requires/0]).

start() ->
	os_ipc:start().

requires() -> [ init_sdict, init_topology ].
