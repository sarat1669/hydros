-module(init_apic).
-export([start/0, requires/0, only_core/0]).

start() ->
	drv_apic:start().

requires() -> [ init_interrupts, init_paging ].

only_core() -> 0.
