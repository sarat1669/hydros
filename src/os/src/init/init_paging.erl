-module(init_paging).
-export([start/0, only_core/0]).

start() ->
	os_paging:init().

only_core() -> 0.
