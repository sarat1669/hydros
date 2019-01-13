-module(init_interrupts).
-export([start/0, requires/0]).

start() ->
	os_interrupts:start(),
	% Ensure that we wait for a period
	os_util:wait(5000).

requires() -> [].

%only_core() -> 0.
