-module(init_normal).
-export([start/0, requires/0]).

start() ->
	os_debug:log("Normal operating mode reached.").

requires() ->
	[ init_terminal, init_single_node, init_multi_node ].
