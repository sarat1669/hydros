-module(init_multi_node_minimal).
-export([start/0, requires/0, only_core/0]).

start() ->
	% Start all other APs.
	%drv_mp_start:all().
	ok.

requires() -> [ init_terminal ].

only_core() -> 0.
