-module(os_root).
-export([start/0]).

%%% This is the first code that runs on the operating system, as local process
%%% zero. Once setting up the capabilities server, the os_init module is
%%% executed.

start() ->
	os_capabilities:start(),
	os_init:boot(),
	receive impossible -> ok end.
