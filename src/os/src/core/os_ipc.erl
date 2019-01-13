-module(os_ipc).
-export([start/0, server/1, send/2, send/3]).
-export([spawn/1, spawn/2, spawn/3]).
-export([cast/1, cast/2, cast/3]).
-export([whereis/2]).
-export([get_status/1]).
-compile({no_auto_import, [{spawn, 1}, {spawn, 2}, {spawn, 3}]}).
-define(BUF_SZ, 16#200000).
-define(TIMEOUT, 0).

%%% This module handles passing messages between nodes in the system
%%% and spawning processes on other nodes.

%%% The informal grammar of the node buffer format used in this file
%%% is as follows:
%%%
%%% Buffer -> Lock Status Messages
%%% Status -> 8 bits
%%% Lock -> 8 bits
%%% Messages  -> Message Messages
%%% Messages  -> Message
%%% Message   -> EndOfList
%%% Message   -> Length TypeTag TermBin
%%% Message   -> Length TypeTag RawBin
%%% Length    -> 8 bits
%%% TypeTag   -> 8 bits
%%% TermBin   -> n*8 bits, where n is the length of the serialized term
%%% RawBin    -> n*8 bits, where n is the length of the binary data
%%% EndOfList -> 0:8 bits
%%%
%%% Binary messages coming from unikernel nodes to multikernel nodes
%%% must use the first four bytes to store an Erlang PID. Messages
%%% from multikernels to unikernels or from unikernels to unikernels
%%% need not adhere to this requirement.

%% The main server state record.
-record(state, {
	proc_id
}).

%% ENUM definitions.
-define(ERLANG_MSG_TYPE, 0).
-define(BIN_MSG_TYPE, 1).

%% Start the server
start() ->
	ProcID = os_system_info:get_proc_id(),
	%reset_buffer(ProcID),
	register(?MODULE,
		erlang:spawn(os_ipc, server,
			[
				#state {
					proc_id = ProcID
				}
			]
		)
	).

%% A wrapper to the send message functionality
send(ToPID, Msg) ->
	os_debug:log("SEND ~p TO ~p.", [Msg, ToPID]),
	?MODULE ! {message, ToPID, Msg}.
send(ToPID, ReplyPID, BinMsg) ->
	BinPID = os_process:pid_to_binary(ReplyPID),
	send(ToPID, << BinPID/binary, BinMsg/binary>>).

%% Spawn on a remote server
spawn(Fun) -> spawn(os_topology:choose_spawn_node(), Fun).
spawn(Fun, Args) when is_function(Fun) ->
	spawn(os_topology:choose_spawn_node(), Fun, Args);
spawn(ToNode, Fun) -> spawn(ToNode, Fun, []).
spawn(ToNode, Fun, Args) ->
	os_debug:log("SPAWN ~p ON ~p WITH ~p", [Fun, ToNode, Args]),
	?MODULE ! {spawn, self(), ToNode, Fun, Args},
	receive
		{spawned, Fun, Args, PID} -> PID
	end.

%% Spawn a process on another node, but do not wait for a PID.
cast(Fun) -> cast(os_topology:choose_spawn_node(), Fun).
cast(Fun, Args) when is_function(Fun) ->
	cast(os_topology:choose_spawn_node(), Fun, Args);
cast(ToNode, Fun) -> cast(ToNode, Fun, []).
cast(ToNode, Fun, Args) ->
	os_debug:log("CAST ~p ON ~p WITH ~p", [Fun, ToNode, Args]),
	?MODULE ! {cast, ToNode, Fun, Args},
	ok.

% A 'global' whereis server.
%whereis(Proc) when is_atom(Proc) -> whereis({local, Proc});
%whereis({N, Proc}) -> whereis(N, Proc).
whereis(local, Proc) -> erlang:whereis(Proc);
whereis(N, Name) ->
	case os_system_info:get_proc_id() of
		N -> whereis(local, Name);
		_ -> do_whereis(N, Name)
	end.

do_whereis(N, Name) ->
	ReplyPID = self(),
	os_debug:log("WHEREIS ~p ON ~p?", [Name, N]),
	os_ipc:spawn(N,
		fun() ->
			os_ipc:send(ReplyPID, {whereis, N, Name, whereis(Name)})
		end
	),
	receive
		{whereis, N, Name, X} -> X
	end.

% The main server loop that handles requests
server(S) ->
	receive
		{spawn, ReplyPID, Target, Fun, Args} ->
			write_message(Target, {spawn, ReplyPID, Fun, Args}, false),
			server(S);
		{cast, Target, Fun, Args} ->
			write_message(Target, {cast, Fun, Args}, false),
			server(S);
		{message, TargetID, Msg} ->
			IsBin = is_integer(TargetID),
			write_message(
				if IsBin -> TargetID;
				true -> os_process:get_node_id(TargetID)
				end,
				if IsBin -> Msg;
				true -> {message, TargetID, Msg}
				end,
				IsBin
			),
			server(S)
	after ?TIMEOUT ->
		handle_buffer(S),
		server(S)
	end.

% Write a message into the appropriate buffer
write_message(TargetID, Msg, IsBin) ->
	case is_buffer_ready(TargetID, Msg, IsBin) of
		false ->
			%os_debug:log("Waiting for message buffer for ~p.", [TargetID]),
			write_message(TargetID, Msg, IsBin);
		true ->
			set_locked(TargetID, true),
			os_unsafe:write(
				detect_buffer_free(TargetID),
				prepare_message(Msg, IsBin)
			),
			os_debug:log("Wrote message into buffer for ~p.", [TargetID]),
			set_locked(TargetID, false)
	end.

%% Create a binary that can be appended to the list in a buffer
prepare_message(Bin, true) ->
	bin_concat(
		bin_concat(<<(size(Bin)):8, 1:8>>, Bin),
		<<0:8>>
	);
prepare_message(Msg, false) ->
	Bin = term_to_binary(Msg),
	bin_concat(
		bin_concat(<<(size(Bin)):8, 0:8>>, Bin),
		<<0:8>>
	).

%% Dirty concatenate binaries. This is slow. We should accept IO lists instead
bin_concat(B1, B2) -> <<B1/binary, B2/binary>>.

%% Find the address in the buffer to write the new message at
detect_buffer_free(TargetID) ->
	% Add one to avoid reading the lock byte (see grammar)
	Res = raw_detect_buffer_free(get_buffer(TargetID) + sizeof(lock) + sizeof(status)),
	%os_debug:log("Mail queue end: ~p.~n", [Res]),
	Res.

get_status(TargetID) ->
	<< Byte:8 >> =
		os_unsafe:read(
			get_buffer(TargetID) + sizeof(lock),
			sizeof(status)
		),
	Byte.

raw_detect_buffer_free(Addr) ->
	case os_unsafe:read(Addr, sizeof(length)) of
		<<0:8>> ->
			% Found the EndOfList symbol
			Addr;
		<<Length:8>> ->
			% Skip to the next Message symbol
			raw_detect_buffer_free(
				Addr + Length + sizeof(length) + sizeof(typetag))
	end.

%% Check whether there is anything to do with the message queue.
%% If so, handle the messages.
%% We currently don't do anything if the queue is locked, but it
%% may be appropriate in the future to wait until the lock is released
%% and check again then.
handle_buffer(S) ->
	case is_locked(S) of
		false -> process_messages(
				 	try parse_raw_messages(S)
					catch _:_ ->
						os_debug:log(warning, "Dropping messages from buffer..."),
						[]
					end
				);
		true -> locked
	end.

%% Perform the tasks described in messages that have been read
%% from the buffer.
process_messages(Msgs) ->
	lists:map(fun process_message/1, Msgs).

process_message({cast, Fun, Args}) ->
	erlang:spawn(fun() -> erlang:apply(Fun, Args) end);
process_message({spawn, ReplyPID, Fun, Args}) ->
	os_ipc:send(ReplyPID,
		{spawned,
			Fun,
			Args,
			erlang:spawn(fun() -> erlang:apply(Fun, Args) end)
		}
	);
process_message({message, TargetPID, Msg}) ->
	TargetPID ! Msg.

%% Return whether or not we can write the message right now.
is_buffer_ready(ProcID, Msg, IsBin) ->
	case {is_locked(ProcID), enough_space(ProcID, Msg, IsBin)} of
		{false, true} -> true;
		_ -> false
	end.

%% Check the status of the buffer pointed to by the process ID
is_locked(S) when is_record(S, state) -> is_locked(S#state.proc_id);
is_locked(ProcID) ->
	<<1:8>> == os_unsafe:read(get_buffer(ProcID), 1).

%% Check that there are enough bytes available in the buffer for the message.
enough_space(ProcID, Msg, IsBin) ->
	(detect_buffer_free(ProcID)
		+ calculate_size(Msg, IsBin)
		- get_buffer(ProcID)) - 1 =< ?BUF_SZ.

%% Calculate the space requirmeent to store a message.
calculate_size(Msg, IsBin) ->
	sizeof(length) + sizeof(typetag) +
		case IsBin of
			true -> byte_size(Msg);
			false -> byte_size(term_to_binary(Msg))
		end.

%% Set the buffer status for the process as locked
set_locked(S, State) when is_record(S, state) ->
	set_locked(S#state.proc_id, State);
set_locked(ProcID, true) ->
	os_util:until(
		fun() ->
			os_unsafe:lock(get_buffer(ProcID))
		end
	);
set_locked(ProcID, false) ->
	os_unsafe:unlock(get_buffer(ProcID)).

%% Get the messages from the buffer and parse them
parse_raw_messages(S) when is_record(S, state) ->
	parse_raw_messages(S#state.proc_id);
parse_raw_messages(ProcID) ->
	set_locked(ProcID, true),
	Msgs =
		lists:filter(
			fun(Msg) -> Msg =/= invalid_message end,
			RawMsgs = do_parse_raw_messages(
						get_buffer(ProcID) + sizeof(lock) + sizeof(status))
		),
	case Msgs == RawMsgs of
		true -> do_nothing;
		false -> 
			os_debug:log(warn, "Threw out ~p invalid message(s)!",
				[length(Msgs) - length(RawMsgs)])
	end,
	reset_buffer(ProcID),
	set_locked(ProcID, false),
	case Msgs of
		[] -> do_nothing;
		_ -> os_debug:log("RECVD ~p.", [Msgs])
	end,
	Msgs.

%% From an address, produce a list of messages
do_parse_raw_messages(Addr) ->
	case os_unsafe:read(Addr, sizeof(length)) of
		<<0:8>> -> [];
		<<Length:8>> ->
			[
				try read_msg(Addr, Length)
				catch _:_ -> invalid_message
				end
			|
				do_parse_raw_messages(
					Addr + Length + sizeof(length) + sizeof(typetag))
			]
	end.

%% Read a message from the message queue.
read_msg(Addr, Length) ->
	case os_unsafe:read(Addr + sizeof(length), sizeof(typetag)) of
		% Handle an Erlang serialised message
		<<?ERLANG_MSG_TYPE:8>> ->
			binary_to_term(
				os_unsafe:read(Addr
					+ sizeof(length) + sizeof(typetag), Length));
		% Handle a binary message
		<<?BIN_MSG_TYPE:8>> ->
			read_binary_msg(Addr
				+ sizeof(length) + sizeof(typetag), Length)
	end.

%% Read the address PID and message
read_binary_msg(Addr, Len) ->
	{message,
		os_process:binary_to_pid(os_unsafe:read(Addr, sizeof(pid))),
		os_unsafe:read(Addr + sizeof(pid) + 1, Len - sizeof(pid))
	}.

%% Truncate the list of messages to zero.
reset_buffer(S) when is_record(S, state) ->
	reset_buffer(S#state.proc_id);
reset_buffer(ProcID) ->
	os_unsafe:write(get_buffer(ProcID) + sizeof(lock) + sizeof(status), <<0:8>>).

%% Get the address of the buffer for a given target
get_buffer(X) -> os_paging:mailbox_addr(X).

sizeof(typetag) -> 1;
sizeof(length) -> 1;
sizeof(lock) -> 1;
sizeof(status) -> 1;
sizeof(pid) -> 8.
