-module(init_start).
-export([start/0, requires/0]).

start() ->
	os_debug:log("HydrOS is booting..."),
	io:format("Starting node ~p of ~p.~n",
		[
			os_system_info:get_proc_id(),
			os_system_info:get_num_procs()
		]
	),
	ok.

%% We can't even write about boot up until the wm has been started.
requires() -> [ init_wm ].

