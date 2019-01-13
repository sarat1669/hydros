-module(os_init).
-export([boot/0]).

%%% This module implements the operating system's init system, which
%%% will execute as local PID 0. The system creates a process for every
%%% goal that it will have to complete in order to reach system readiness.
%%% These processes then run concurrently, under the supervision of the
%%% os_init process.
%%%
%%% Each goal that is implemented must be done in the following way.
%%% Goals can be added by other goals, if neccessary, during execution,
%%% but the main goal must be available for execution to start.
%%%
%%% All init targets must have their own 'init_xxxx' module, which implements
%%% the following interface:
%%% 	- start/0
%%% 	- requires/0
%%% 	- stop/0
%%% 	- only_core/0
%%% All of the functions are optional. If start or stop are not provided, the
%%% system will not perform any action when the start/stop command is
%%% performed. If requires/0 is missing, it is assumed that no other
%%% goals are required for the execution of the present goal. only_core/0
%%% specifies a single core that the goal is run on, if provided.
%%%
%%% TODO: Implement stopping of started services!

-define(GOAL, init_cli).

%% This defines the supervisor state.
-record(super, {
	goal = ?GOAL,
	type = start,
	workers = []
}).

%% This defines a worker process' state
-record(worker, {
	name = undefined, % The name of the goal
	master = undefined, % The init supervisor process
	wait = [], % We are waiting for these processes to complete
	tell = [], % We tell these processes, once we are done
	done = false % Whether the function has been executed yet
}).

%% Start the init service.
%% Make a supervisor server state and spawn the goal function.
boot() ->
	register(
		?MODULE,
		spawn(
			fun() ->
				super(undefined)
			end
		)
	),
	?MODULE ! {set_goal, ?GOAL}.

%%% SUPERVISOR PROCESS API

%%% WORKER PROCESS API

%% Get the PID of the process that is looking afcter the goal.
goal(Name) ->
	?MODULE ! {get_goal, self(), Name},
	receive
		{goal, Name, PID} -> PID
	end.
	
%%% The supervisor process

%% This server answers by worker processes to receive the PID of a given
%% goal. One is created if it does not exist.
super(undefined) ->
	receive
		{set_goal, G} ->
			%% The super proc needs to be registered
			%% before we can setup the state,
			%% subsequently the super function needs
			%% to handle self-setup.
			super(element(1, find_goal(#super{}, G)))
	end;
super(S) ->
	receive
		%% Return a PID that manages the requested goal
		{get_goal, Reply, Name} ->
			{NewS, PID} = find_goal(S, Name),
			Reply ! {goal, Name, PID},
			super(NewS)
	end.

%%% Supervisor helper functions

%% Return a PID (or 'finished') for a given goal
find_goal(S, Name) ->
	case lists:keyfind(Name, 1, S#super.workers) of
		{Name, PID} -> {S, PID};
		false -> add_goal(S, Name)
	end.

%% Add a new goal directly to the supervisor's list
add_goal(#super { workers = Gs }, Name) ->
	NS = #super { workers = [{Name, G = new_goal(Name)}|Gs] },
	{NS, G}.

%% Spawn a new proc for a goal. Importantly, this
%% function also sends messages the supervisor,
%% making it spawn new processes for pre-reqs.
new_goal(Name) ->
	Master = self(),
	spawn(
		fun() ->
			Ws =
				lists:map(
					fun goal/1,
					(get_function(Name, requires))()
				),
			lists:foreach(fun ready/1, Ws),
			worker(
				#worker{
					name = Name,
					master = Master,
					wait = Ws
				})
		end
	).

%% Get the requirements for a goal
get_function(Name, Fun) ->
	case lists:keyfind(Fun, 1, Name:module_info(exports)) of
		%% For undefined funs, returning [] works
		%% for all possibilities (start, stop, requires).
		false -> fun() -> [] end;
		_ -> fun Name:Fun/0
	end.

%%% The worker process
worker(S = #worker {tell = T}) ->
	receive
		% If we are done, reply immediately, otherwise wait
		{ready, PID} ->
			case done(S) of
				false ->
					worker(S#worker { tell = [PID|T] });
				true ->
					tell(PID),
					worker(S);
				ready ->
					NewS = maybe_exec(S),
					tell(PID),
					worker(NewS)
			end;
		% If we receive a 'done' message, execute func
		% and tell those waiting on us.
		{done, PID} ->
			worker(process_done(S, PID))
	end.

%%% Worker helper functions

%% Ask goal whether it has completed
ready(PID) -> PID ! {ready, self()}.

%% Return true, when the worker is finished
done(#worker { done = true }) -> true;
done(#worker { wait = [], done = false }) -> ready;
done(_) -> false.

%% Tell a process that we are done
tell(PID) -> PID ! {done, self()}.

%% Process a 'done' message from another process
process_done(S = #worker { wait = W }, PID) ->
	maybe_exec(S#worker {wait = W -- [PID] }).

%% Execute the function, if appropriate
maybe_exec(S = #worker { done = true }) -> S;
maybe_exec(S = #worker { name = N, wait = [], tell = Ts}) ->
	case correct_core(N) and first_boot(N) of
		true -> execute_goal(N);
		false -> do_nothing
	end,
	lists:foreach(fun tell/1, Ts),
	S#worker { wait = [], tell = [], done = true };
maybe_exec(S) -> S.

%% Detect whether this is an appropriate core for execution.
correct_core(M) ->
	case lists:keyfind(only_core, 1, M:module_info(exports)) of
		false -> true;
		_ -> os_system_info:get_proc_id() == (M:only_core())
	end.

%% Check whether this function is only supposed to be run on
%% the first boot of the system, and whether this is the first
%% boot.
first_boot(M) ->
	case lists:keyfind(only_first_boot, 1, M:module_info(exports)) of
		false -> true;
		_ -> 
			M:only_first_boot() and os_system_info:is_first_boot()
	end.

%% Actually execute the goal
execute_goal(N) ->
	%erlang:display({executing_goal, N}),
	%io:format("Executing goal ~p...~n", [N]),
	os_debug:log(5, "init: Executing goal ~p", [N]),
	(get_function(N, start))().
