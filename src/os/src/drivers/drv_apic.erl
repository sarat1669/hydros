-module(drv_apic).
-export([start/0]).

%%% This module handles starting the apic subsystem.
%%% This task must only be completed on the first subnode
%%% to come up.

start() ->
	os_unsafe:instruction(cli),
	drv_pic:disable(),
	drv_pic:eoi(),
	drv_lapic:start(),
	drv_ioapic:start(),
	os_unsafe:instruction(sti).


