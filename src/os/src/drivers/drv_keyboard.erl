-module(drv_keyboard).
-include("keymaps.hrl").
-export([start/0, add_listener/1, add_exclusive_listener/1]).
-export([remove_listener/1]).

-record(state, {
	listeners = [],
	exclusive = undefined,
	last = 0,
	escape_code = undefined,
	down_keys = []
}).

-define(KBD_INTERRUPT, 16#21).
-define(SCANCODE_TAB, kbd_scancodes).

%% Start the keyboard driver.
start() ->
	register(?MODULE,
		PID = spawn(
			fun() ->
				create_scancode_tab(),
				server(#state{})
			end
		)
	),
	os_interrupts:add_listener(?KBD_INTERRUPT, PID),
	reset(),
	PID.

%% Register a process that would like to receive keys.
add_listener(PID) ->
	?MODULE ! {add, PID},
	ok.

%% Register a process that receives all the keys, exclusively.
add_exclusive_listener(PID) ->
	?MODULE ! {add_exclusive, PID},
	ok.

%% No longer receive key information.
remove_listener(PID) ->
	?MODULE ! {remove, PID},
	ok.

%% SERVER
server(S) ->
	receive
		{add, PID} ->
			os_debug:log(2, "Adding listener ~p to kbd.", [PID]),
			server(S#state { listeners = [PID|S#state.listeners] });
		{add_exclusive, PID} ->
			os_debug:log(2, "Adding exclusive listener ~p to kbd.", [PID]),
			server(S#state { exclusive = PID });
		{remove, PID} ->
			os_debug:log(2, "Removing ~p from kbd handler.", [PID]),
			server(
				case S#state.exclusive of
					PID -> S#state { exclusive = undefined };
					_ ->
						S#state {
							listeners = S#state.listeners -- [PID]
						}
				end
			);
		{interrupt, 16#21, _, _ } ->
			% We have received a keyboard interrupt.
			% Process it, then forward the key info to listeners
			server(process_scancode(S, read()));
		{set_keymap, Locale} ->
			set_keymap(Locale)
	end.

%% PRIVATE HELPER FUNCTIONS

%% Reset the driver.
reset() ->
	case drv_port:in(16#64) band 1 of
		1 ->
			os_debug:log(warn, "Clearing key from buffer."),
			drv_port:in(16#60),
			reset();
		0 -> ok
	end.

%% Inform the appropriate listeners of the new keys.
inform_listeners(#state{ exclusive = PID }, Key)
		when PID =/= undefined ->
	inform_listener(PID, Key);
inform_listeners(#state{ listeners = Ls }, Key) ->
	lists:foreach(fun(PID) -> inform_listener(PID, Key) end, Ls).

%% Notify one listener

inform_listener(_, ignore) -> do_nothing;
inform_listener(PID, Key) ->
	os_debug:log(1, "kbd dispatching: ~p to ~p. Alive? ~p",
		[Key, PID, erlang:is_process_alive(PID)]),
	PID ! {key, Key}.

%% Read a scancode from the keyboard controller.
read() -> os_unsafe:port_in(16#60).

%% Take the server state and a scancode, then return a new state
%% aswell as keys that are down.
process_scancode(S, Code = 16#E0) ->
	% This is an escape code, change state and return to the server
	S#state { escape_code = Code };
process_scancode(S, Code) ->
	PrimaryCode = Code band 16#7f,
	Key =
		extract_key(
			ets:lookup(
				?SCANCODE_TAB,
				case S#state.escape_code of
					undefined -> [PrimaryCode];
					EscapeCode -> [EscapeCode, PrimaryCode]
				end
			)
		),
	NewDownKeys = get_down_keys(S, Key, Code==PrimaryCode),
	inform_listeners(S, process_down_keys(NewDownKeys)),
	%os_debug:log("Key: ~p. Down keys: ~p. RESULT: ~p.", [NewDownKeys, Key, NewDownKeys, Final]),
	maybe_change_layout(NewDownKeys),
	S#state {
		down_keys = NewDownKeys,
		escape_code = undefined
	}.

%% Finds the next char to deliver to listening processes
process_down_keys(Keys) ->
	{XKeys, Mods} =
		lists:splitwith(
			fun(#key{ type = Type }) -> Type =/= mod end,
			Keys
		),
	generate_char(XKeys, Mods).

%% Apply modifiers to pressed characters, returning the
%% actual input character.
generate_char([], []) -> ignore;
generate_char([], [Mod|_]) ->
	hd(Mod#key.key_characters);
generate_char([Key|_], Mods) ->
	os_debug:log("Key: ~p (mods: ~p)", [Key, Mods]),
	case {Key#key.key_characters,
		    lists:usort(lists:flatten([ M#key.key_characters || M <- Mods]))} of
		{[{C, _}], _} -> C;
		{[C], _} -> C;
		{[C, _], []} -> C;
		{[_, C|_], [shift]} -> C;
		{[_, _, C, _, _, _, _, _], [ctrl]} -> C;
		{[_, _, _, _, C, _, _, _], [alt]} -> C;
		{[_, _, _, _, _, _, C, _], [alt, ctrl]} -> C;
		_ ->
			os_debug:log("Ignoring key ~p.", [Key]),
			ignore
	end.

%% Returns a list of #keys that are currently down,
get_down_keys(S, unknown, _Down) -> S#state.down_keys;
get_down_keys(#state { down_keys = Down }, Key, true) ->
	case lists:keymember(Key#key.scancode, #key.scancode, Down) of
		true -> Down;
	 	false -> [Key|Down]
	end;
get_down_keys(#state { down_keys = Down }, Key, false) ->
	lists:keydelete(Key#key.scancode, #key.scancode, Down).

%% Process the response from an ets query.
extract_key([{_Scancode, Key}|_]) -> Key;
extract_key(_) -> unknown.

%% Change keyboard layout if DownKeys is alt+shift+<valid locale>
maybe_change_layout(DownKeys) when length(DownKeys) == 4 ->
	Chars = [ hd(K#key.key_characters) || K <- lists:reverse(DownKeys) ],
	os_debug:log("~w", [Chars]),
	case Chars of
		[alt, shift, K1, K2] ->
			try
				Locale = [K1, K2],
				set_keymap(Locale)
			catch
				error:badarg -> ok
			end;
		_ ->
		  ok
	end;
maybe_change_layout(_DownKeys) -> ok.

%% Set keymap as indicated by Locale (see keymaps.hrl)
set_keymap(Locale) ->
	os_debug:log("Supported keymaps: ~p", [[ KM || {KM, _} <- ?KEYMAPS]]),
	case lists:keyfind(Map = string:to_lower(Locale), 1, ?KEYMAPS) of
		{Locale, Layout} ->
			os_debug:log("Setting keymap ~p",[Locale]),
			ets:insert(?SCANCODE_TAB, [ { K#key.scancode, K } || K <- Layout ] );
		_ ->
			os_debug:log(error, "Cannot set keymap ~p.", [Map]),
			ok
	end.

%% Creates a table containing all of the data required to convert scancodes.
create_scancode_tab() ->
	ets:new(?SCANCODE_TAB, [set, named_table, public]),
	set_keymap(?DEFAULT_KEYMAP).
