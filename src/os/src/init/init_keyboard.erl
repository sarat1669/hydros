-module(init_keyboard).
-export([start/0, requires/0]).

start() ->
	drv_keyboard:start().

requires() -> [ init_interrupts, init_apic ].

%only_core() -> 0.
