-module(os_interrupts).
-export([start/0, add_listener/2, remove_listener/2]).

%%%  This module handles distributing interrupts.
%%%  The function registers to receive interrupts from the OS, then
%%%  sends these interupts to registered processes.

-record(state, {
	ints = []
}).

%% Public API

start() ->
	register(?MODULE, PID = spawn(fun() -> server(#state{}) end)),
	os_unsafe:register_for_interrupts(PID),
	ok.

add_listener(Int, Proc) ->
	?MODULE ! {add, Int, Proc},
	ok.

remove_listener(Int, Proc) ->
	?MODULE ! {remove, Int, Proc},
	ok.

%% Server

server(S) ->
	receive
		{add, Int, Proc} ->
			server(S#state { ints = add(S#state.ints, Int, Proc) });
		{remove, Int, Proc} ->
			server(S#state { ints = remove(S#state.ints, Int, Proc)});
		Int ->
			%os_util:wait(1000),
			send_ints(Int, S#state.ints),
			server(S)
	end.

%% Helper functions

add([], Int, Proc) -> [{Int, [Proc]}];
add([{Int, Procs}|R], Int, Proc) -> [{Int, [Proc|Procs]}|R];
add([H|R], Int, Proc) -> [H|add(R, Int, Proc)].

remove([], Int, Proc) -> [{Int, [Proc]}];
remove([{Int, Procs}|R], Int, Proc) -> [{Int, Procs -- [Proc]}|R];
remove([H|R], Int, Proc) -> [H|remove(R, Int, Proc)].

send_ints({Int, ErrNum, Addr}, Ints) ->
	case lists:keyfind(Int, 1, Ints) of
		{Int, Procs} ->
			lists:foreach(
				fun(Proc) ->
					Proc ! {interrupt, Int, ErrNum, Addr}
				end,
				Procs
			);
		false -> ok
	end.
