-module(init_serial).
-export([start/0, requires/0, only_core/0]).

start() ->
	drv_serial:init(1),
	io:format("Started serial device 1.~n"),
	spawn(fun test/0),
	io:format("Sent test data on serial dev 1.~n"),
	ok.

test() ->
	drv_serial:write(1, $A),
	test().

requires() -> [ init_terminal, init_interrupts, init_apic ].

only_core() -> 0.
