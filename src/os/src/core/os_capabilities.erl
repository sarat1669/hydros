-module(os_capabilities).
-compile({no_auto_import, spawn}).
-compile({no_auto_import,[get/0, get/1]}).
-export([start/0, spawn/2]).
-export([validate/1]).
-export([validate/3, validate_fail/3, validate_err/3]).
-export([validate_apply/4, validate_apply/5]).
-export([get/0, get/1]).
-export([start_server/1]).
-export([info/1]).

-include("cap.hrl").

-define(TAB, capabilities_tab).

%%% Defines the capabilities system of the operating system. This provides a
%%% mechanism of access controls for processes, creating a 'capabilites tree'
%%% in which the subtrees of any given tree have equal or fewer capabilities
%%% than thier parents.
%%%
%%% The system stores capabilities 'negatively' -- that is, the capability
%%% [] means that a process may do anything, [abc] means that a process cannot
%%% perform action 'abc'.
%%%
%%% Spawns no longer require a round of scheduling in order for the child 
%%% process to come up, this is, however, at the expense of simplicity.
%%% Now, only processes which have asked to perform some privelidged action
%%% before the cap server has registered their status are required to wait for
%%% the server to be scheduled.
%%%
%%% Roughly, this works as follows:
%%% When the spawn function is called, the spawner BIF is called directly.
%%% A 'store' message is then sent to the capabilities server and the PID
%%% is returned. If a process that has not yet been entered into the table
%%% attempts to perform a privelidged action, an 'available' message is sent
%%% to the server. Because 'store' messages are always prioritised over
%%% 'available' messages. The 'available' message will only be processed after
%%% the process's 'store' messages. Subsequently, when the 'available' message
%%% has been responded to, the store message of the process must have been
%%% processed. The (now ready) capabilites are then requested from the table.

%% Start the processing server
start() ->
	Root = self(),
	case lists:member(?MODULE, erlang:registered()) of
		true -> error(already_running);
		false ->
			register(
				?MODULE,
				erlang:do_spawn(
					?MODULE,
					start_server,
					[Root]
				)
			)
	end.

%% Setup the table, the root entry and begin execution
start_server(Root) ->
	ets:new(?TAB, [set, protected, named_table]),
	ets:insert(?TAB, {Root, []}),
	server().

%% Spawn a process with the same capabilities as the caller,
%% with the additions.
spawn(Fun, Caps) ->
	PID = erlang:do_spawn(erlang, apply, [Fun, []]),
	?MODULE ! {store, self(), PID, expand_caps(Caps)},
	PID.

%% Takes a list of caps as given to spawn/2.
%% This expands various short-form capabilities into full
%% capability objects.
expand_caps(Cap) when is_record(Cap, capability) ->
	expand_caps([Cap]);
expand_caps([]) -> [];
expand_caps([Cap|R]) when is_record(Cap, capability) ->
	[ Cap | expand_caps(R) ];
expand_caps([Atom|R]) when is_atom(Atom) ->
	[ #capability { name = Atom, type = allow } | expand_caps(R)];
expand_caps([{Am, Type}|R]) ->
	[ #capability { name = Am, type = Type } | expand_caps(R)];
expand_caps([{Am, Type, Details}|R]) ->
	[
		#capability{
		   name = Am,
		   type = Type,
		   details = Details
		}
	|
		expand_caps(R)
	].

%% Print human readable info about the capabilities associated with a
%% module, if it has the correct tags.
info(Mod) ->
	io:format("Capability information for module: ~p.~n", [Mod]),
	io:format("Capability name: ~p.~n", [get_attr(Mod, capability)]),
	io:format("Default behaviour: ~p.~n", [get_attr(Mod, cap_default)]),
	io:format("On failure: ~p.~n", [get_attr(Mod, cap_rejection)]),
	ok.

%% Get an attr (or undefined) from module information.
get_attr(Mod, Attr) ->
	case lists:keyfind(Attr, 1, Mod:module_info(attributes)) of
		false -> undefined;
		{Attr, Val} -> Val
	end.

%% Wait until the server is available to perform an action.
wait_available() ->
	?MODULE ! {available, Ref = make_ref(), self()},
	receive
		{available, Ref} ->
			ok
	end.

%% Respond to spawn requests. These requests muct go through this server, as
%% it alone has the ability to add/modify capabilities.
server() ->
	receive
		{store, Parent, Child, WoCaps} ->
			do_store(Parent, Child, WoCaps),
			server()
	end.

do_store(Parent, Child, WoCaps) ->
	ets:insert(?TAB, {Child, adjust(caps(Parent), WoCaps)}),
	receive
		{available, Ref, Child} ->
			Child ! {available, Ref}
	after 0 -> ok
	end.

%% Adjust the capabilities of a process. There is a strong likelihood that we
%% will end up making this greatly more complex in the future, hence the
%% abstraction.
adjust(Caps, WoCaps) -> Caps ++ WoCaps.

%% Retreive the capabilities for a process from the table.
caps(PID) ->
	case ets:lookup(?TAB, PID) of
		[{PID, Caps}] -> Caps;
		[] -> undefined
	end.

%% Get the capabilities for the current process.
get() ->
	case caps(self()) of
		undefined ->
			% There is nothing currently stored for this process, wait
			% for the server to become available and try again, if that
			% fails, this is a rogue process and needs to be killed.
			wait_available(),
			case caps(self()) of
				undefined -> error(no_capabilities_stored);
				Caps -> Caps
			end;
		Caps -> Caps
	end.

%% Get the capabilitiesthat have CapName for the current process.
get(CapName) ->
	[ X || X = #capability { name = CapName } <- get() ].

%% Check whether a process can perform the generic task that it is attempting to
%% These capabilites are negative -- that is, we are checking for
%% the non-existance of the entry in the list to ensure that an action
%% can be performed.
%%
%% This function will crash the process if the check fails.
%validate(X) when not is_list(X) -> validate([X]);
%validate(WoCaps) -> do_validate(get(), WoCaps).
			
%% Now that the ProcCaps and WoCaps are both in hand, actially perform
%% the test.
%do_validate(ProcCaps, WoCap) ->
%	case ProcCaps -- WoCap of
%		ProcCaps -> ok;
%		_ -> error({no_capability, WoCap})
%	end.

%% Take a capability type name, a details validation function and
%% some args.
%%
%% Validation functions take call-specific data (normally function args)
%% and a capability. They return true or false.
validate(Name) ->
	do_validate(Name, fun([], _Cap) -> true end, []).
validate(Name, MatchFun, Args) ->
	do_validate(MatchFun, Args, get(Name)).

do_validate(_MatchFun, _Args, []) -> true;
do_validate(MatchFun, Args, [Cap = #capability{type = disallow}|R]) ->
	% Handle negative capabilities
	case MatchFun(Args, Cap) of
		true -> false;
		false -> do_validate(MatchFun, Args, R)
	end;
do_validate(MatchFun, Args, [Cap = #capability{type = allow}|R]) ->
	case MatchFun(Args, Cap) of
		true -> true;
		false -> do_validate(MatchFun, Args, R)
	end;
do_validate(MatchFun, Args, [#capability{ type = _ }|R]) ->
	do_validate(MatchFun, Args, R).

%% Validate the capabilities, failing on 'false'.
validate_fail(Name, MatchFun, Args) ->
	case validate(Name, MatchFun, Args) of
		true -> ok;
		false -> error(capabilities)
	end.

%% Validate the capabilities, returning '{error, capabilities}' on false.
validate_err(Name, MatchFun, Args) ->
	case validate(Name, MatchFun, Args) of
		true -> ok;
		false -> {error, capabilities}
	end.

%% Execute a function (optionally with args) if the validator returns true.
validate_apply(Name, MatchFun, Args, Fun) ->
	validate_apply(Name, MatchFun, Args, Fun, []).
validate_apply(Name, MatchFun, Args, Fun, FunArgs) ->
	case validate(Name, MatchFun, Args) of
		true -> apply(Fun, FunArgs);
		false -> error(capabiltities)
	end.
