-module(init_cli).
-export([start/0, requires/0, only_core/0]).

start() ->
	% Start an auxillary, backup console.
	app_console:start("Emergency"),
	% Start the basic console
	app_console:start_default().

requires() -> [ init_paging, init_terminal, init_multi_node, init_wm, init_ipc ].

only_core() -> 0.
