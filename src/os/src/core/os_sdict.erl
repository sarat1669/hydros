-module(os_sdict).
-export([start/0, new/1, keys/1, find/2, store/3]).

%%% A basic, generalised key-value dictionary that uses ets tables that are
%%% shared across an OS node. Impementing node-local data lookup this way
%%% is substantially more efficient that using the normal server method.

start() ->
	register(?MODULE, spawn(fun server/0)).

new(Name) ->
	?MODULE ! {new, self(), Name},
	receive
		{?MODULE, ok} -> ok
	end.

server() ->
	receive
		{new, PID, Name} ->
			ets:new(Name, [set, public, named_table]),
			PID ! {?MODULE, ok},
			server()
	end.

find(Dict, Key) ->
	case ets:lookup(Dict, Key) of
		[{Key, Obj}] -> Obj;
		[] -> not_found
	end.

keys(Dict) ->
	ets:foldl(fun({X,_}, Acc) -> Acc ++ [X] end, [], Dict).

store(Dict, Key, Val) -> ets:insert(Dict, {Key, Val}).
