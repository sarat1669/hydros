-module(init_pci).
-export([start/0, requires/0]).

start() ->
	%drv_pci:start().
	ok.

requires() -> [].
