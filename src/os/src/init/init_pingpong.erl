-module(init_pingpong).
-export([start/0, start/1, requires/0, only_core/0]).
-export([server/0, server/1]).

start() -> start(1).
start(Proc) ->
	io:format("Started pinging!~n"),
	% Spawn the pingpong servers
	RemotePID = os_ipc:spawn(Proc, fun server/0, []),
	io:format("Spawned remote!~n"),
	LocalPID = spawn(fun server/0),
	io:format("Spawned local!~n"),
	% Ping the remote server, with a return PID of the local server
	ping(RemotePID, LocalPID),
	ok.

server() -> server(0).
server(X) ->
	os_debug:log("Ping server waiting (~p).", [X]),
	receive
		{ping, PID} ->
			io:format("Received ping ~p from ~p.~n", [X, PID]),
			os_util:wait(10000000),
			ping(PID),
			os_debug:log("Pinged!"),
			server(X + 1)
	after 1 ->
			io:format("Waiting for pong ~p.~n", [X]),
			server(X)
	end.

ping(PID) -> ping(PID, self()).
ping(PID, Self) ->
	os_debug:log(3, "Pinging ~p from ~p.~n", [PID, Self]),
	os_ipc:send(PID, {ping, Self}).

requires() -> [ init_terminal, init_ipc ].

only_core() -> 1.
