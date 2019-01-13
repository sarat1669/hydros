-module(init_smbios).
-export([start/0, requires/0]).

start() ->
	drv_smbios:start().

requires() -> [ init_sdict ].
