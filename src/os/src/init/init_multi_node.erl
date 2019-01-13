-module(init_multi_node).
-export([start/0, requires/0, only_core/0, only_first_boot/0]).

start() ->
	% Start all other APs.
	drv_mp_start:all().

requires() -> [ init_terminal, init_paging ].

only_core() -> 0.

only_first_boot() -> true.
