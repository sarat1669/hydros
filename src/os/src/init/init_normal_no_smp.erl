-module(init_normal_no_smp).
-export([start/0, requires/0]).

start() ->
	os_debug:log("No SMP boot complete.").

requires() ->
	[ init_terminal, init_cli, init_single_node, init_ipc ].
