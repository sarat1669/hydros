-module(os_lli).
-export([run/1, supported/0]).

run(_Instructions) -> erlang:nif_error(undefined).

supported() ->
	[
		read,
		write,
		move
	].
