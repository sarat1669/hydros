-module(os_topology).
-export([start/0, choose_spawn_node/0]).

%%% This module currents acts as the store for information about other nodes.
%%% At this time, it only keeps track of the number of processes located on
%%% each other node. It also provides a function that chooses the node on
%%% which to place a new, not directly mapped process.

-define(LOAD_TAB, node_loads).
-define(STATUS_TAB, node_statuses).
-define(UPDATE_TIME, 1000).

-record(state, {
	clients = []
}).

start() ->
	register(?MODULE, spawn(fun() -> start_server() end)).

choose_spawn_node() ->
	% Wait until the topology server has actually started
	os_util:until(
		fun() ->
			lists:member(?LOAD_TAB, ets:all())
		end
	),
	element(1,
		hd(
			lists:keysort(
				2,
				lists:map(
					fun(X) ->
						{X, os_sdict:find(?LOAD_TAB, X)}
					end,
					os_sdict:keys(?LOAD_TAB)
				) ++
				[
					{
						os_system_info:get_proc_id(),
						length(erlang:processes())
					}
				]
			)
		)
	).
	

start_server() ->
	os_sdict:new(?LOAD_TAB),
	os_sdict:new(?STATUS_TAB),
	Nodes =
		(lists:seq(0, os_system_info:get_num_procs() - 1)
			-- [os_system_info:get_proc_id()])
			-- [ UniKID || {UniKID, _} <- os_unikernels:kernels() ],
	lists:map(fun until_ready/1, Nodes),
	erlang:display({nodes_found, Nodes}),
	set_timers(),
	server(
		#state {
			clients =
				lists:map(
					fun(X) -> os_ipc:whereis(X, ?MODULE) end,
					Nodes
				)
	}).

server(S = #state { clients = Cs }) ->
	receive
		load_update ->
			%erlang:display({sending_load, Cs}),
			lists:foreach(
				fun(C) ->
					os_ipc:send(
						C,
						{load_update,
							os_system_info:get_proc_id(),
							length(erlang:processes())}
					)
				end,
				Cs
			),
			set_timers(),
			server(S);
		node_states ->
			lists:foreach(
				fun(Node) ->
					spawn(fun() -> update_node_status(Node) end)
				end,
				lists:seq(0, os_system_info:get_num_procs() - 1)
			),
			server(S);
		{load_update, Node, X} ->
			%erlang:display({got_load_from, Node, X}),
			os_sdict:store(?LOAD_TAB, Node, X),
			os_util:if_not_core(0, fun() -> self() ! load_update end),
			server(S)
	end.

until_ready(Node) ->
	case os_ipc:whereis(Node, ?MODULE) of
		undefined ->
			os_util:wait(100),
			until_ready(Node);
		X -> X
	end.

set_timers() ->
	os_util:if_core(0,
		fun() ->
			erlang:send_after(?UPDATE_TIME, self(), load_update),
			erlang:send_after(?UPDATE_TIME, self(), node_states)
		end
	).

update_node_status(Node) ->
	update_node_status(Node, os_ipc:get_status(Node)).
update_node_status(Node, First) ->
	Status = 
		receive after 1000 ->
			case os_ipc:get_status(Node) of
				First -> unresponsive;
				_ -> ok
			end
		end,
	os_sdict:store(?STATUS_TAB, Node, Status).
