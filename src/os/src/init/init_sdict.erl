-module(init_sdict).
-export([start/0, requires/0]).

start() ->
	os_sdict:start().

requires() -> [].
