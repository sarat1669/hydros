-module(init_time).
-export([start/0, requires/0]).
-export([only_core/0]).

start() ->
	drv_timer:start().

requires() -> [ init_interrupts, init_apic ].

only_core() -> 0.
