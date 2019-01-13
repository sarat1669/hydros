-module(init_terminal).
-export([start/0, requires/0]).

start() ->
	wm_terminal:start_default().

requires() -> [].
