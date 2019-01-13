-module(wm).
-export([start/0]).
-export([app/2, app/3, add_app/2, add_app/3]).
-export([remove_app/1]).
-export([focus/1, list_apps/0]).

-include("app.hrl").

%%% The HydrOS window manager and compositor.

-record(state, {
	apps = [],
	focused = undefined
}).

-define(DEFAULT_APP_NAME, "Unnamed App").

%% Start the HydrOS window manager
start() ->
	os_debug:log(5, "Starting Window Manager..."),
	register(?MODULE, PID = spawn(fun() -> server(#state {}) end)),
	drv_keyboard:add_listener(PID),
	ok.

%% Start a fullscreen HydrOS app and bring it to the fore.
app(AppPID, WinPID) ->
	app(AppPID, WinPID, ?DEFAULT_APP_NAME).
app(AppPID, WinPID, Name) ->
	add_app(AppPID, WinPID, Name, true),
	ok.

%% Add a running application process, and associated window, to
%% the list of running applications
add_app(AppPID, WinPID) ->
	add_app(AppPID, WinPID, ?DEFAULT_APP_NAME).
add_app(AppPID, WinPID, Name) ->
	add_app(AppPID, WinPID, Name, false).
add_app(AppPID, WinPID, Name, Focused) ->
	os_debug:log(3, "Adding app '~s' with app ~p and window ~p. Focused: ~p.",
		[Name, AppPID, WinPID, Focused]),
	?MODULE ! {add, AppPID, WinPID, Name, Focused},
	ok.

%% Remove the app with the matching PID from the app list
remove_app(PID) ->
	?MODULE ! {remove, PID},
	ok.

%% Unfocus the previously selected app, and select this one.
focus(PID) ->
	?MODULE ! {focus, PID},
	ok.

%% Returns a list of the applications registered with the window manager
list_apps() ->
	?MODULE ! {list, self()},
	receive
		{list, Apps} -> Apps
	end.

server(S) ->
	receive
		%% Respond to requests
		{add, App, Win, Name, Focused} ->
			os_debug:log(
				2,
				"Adding app ~s (with app proc: ~p, and window: ~p. F? ~p).",
				[ Name, App, Win, Focused ]
			),
			NewS =
				S#state {
					apps =
						[
							#app{
								name = Name,
								app = App,
								window = Win
							}
						|
							S#state.apps
						]
				},
			server(if Focused -> focus(NewS, App); true -> NewS end);
		{remove, PID} ->
			os_debug:log(3, "Removing app ~p from WM.", [PID]),
			NewS =
				S#state {
					apps =
						S#state.apps -- [find_by_pid(S, PID)]
				},
			server(
				case S#state.focused of
					PID -> next_window(NewS);
					_ -> NewS
				end
			);
		{focus, PID} ->
			os_debug:log(2, "Focusing app ~p...", [PID]),
			server(focus(S, PID));
		{list, PID} ->
			os_debug:log(2, "Generating app list for ~p. Apps: ~p.",
				[PID, S#state.apps]),
			PID ! {list, S#state.apps},
			server(S);
		%% Respond to keyboard input
		{key, f12} ->
			os_debug:log("WM starting window switcher."),
			app_window_switcher:start(),
			server(S);
		{key, f11} ->
			os_debug:log("WM starting the log viewer."),
			app_logs:start(),
			server(S);
		{key, f9} ->
			os_debug:log(important, "Attempting debug shutdown!"),
			os_debug:shutdown(),
			server(S);
		%% Ignore all other keys
		{key, _} -> server(S);
		X ->
			os_debug:log(warning, "WM got unknown message ~p.", [X]),
			server(S)
	end.

%% PRIVATE HELPER FUNCTIONS

%% Trigger an application to start drawing to the screen,
%% aswell as receiving keyboard input.
focus(S, PID) ->
	case find_by_pid(NewS = unfocus(S), PID) of
		false ->
			os_debug:log(2, "Could not focus ~p. PID not registered with WM.",
				[PID]),
			NewS;
		App ->
			os_debug:log(3, "WM trying to focus app ~p.", [App#app.app]),
			% Register the app's main pid to receive keyboard keys
			drv_keyboard:add_listener(App#app.app),
			% Tell the window to start blitting automatically
			wm_window:set_focused(App#app.window, true),
			% Return the new server state, with focused modified
			NewS#state { focused = PID }
	end.

%% Stop an application from receiving input and blitting to screen.
%% No new buffer will be automatically printed, so the contents of
%% the buffer will be left on screen until explicitly replaced.
unfocus(S = #state { focused = undefined }) ->
	os_debug:log(1, "No app is focused, so unfocus not required."),
	S;
unfocus(S = #state { focused = PID }) ->
	% Remove the keyboard listener.
	drv_keyboard:remove_listener(PID),
	% If the process is alive and attached to the WM, set it unfocused.
	case {erlang:is_process_alive(PID), find_by_pid(S, PID)} of
		{false, _} -> do_nothing;
		{true, false} -> do_nothing;
		{true, App} ->
			wm_window:set_focused(App#app.window, false)
	end,
	S#state { focused = undefined }.

%% Takes a server state and pid, returns the matching app
find_by_pid(S, PID) ->
	lists:keyfind(PID, #app.app, S#state.apps).

%% Switch context to the next window. The equivelant of alt-tab.
next_window(S = #state { apps = [] }) -> unfocus(S);
next_window(S = #state { apps = [Next|AppList] }) ->
	(focus(unfocus(S), Next#app.app))#state {
		apps = AppList ++ [Next]
	}.
