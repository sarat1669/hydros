-module(init_minimal).
-export([start/0, requires/0]).

start() ->
	os_debug:log("OS components initialised."),
	ok.

requires() ->
	[
		init_terminal
%		init_multi_node_minimal
	].
