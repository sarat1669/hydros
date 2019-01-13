-module(init_single_node).
-export([start/0, requires/0]).

start() ->
	os_debug:log("OS components initialised.").

requires() ->
	[
		init_terminal,
		init_start,
		init_interrupts,
		init_time,
		init_smbios,
		init_pci
	].
