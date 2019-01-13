-module(app_window_switcher).
-export([start/0]).

-include("app.hrl").

%% Define the name of the window and application
-define(NAME, "Window Switcher").
%% The amount of time before screen redraws
-define(REFRESH_TIME, 1000).

%%% Defines a basic application that allows the user to view and choose
%%% another window to focus.

-record(state, {
	window,
	app_list
}).

%% Ensure that the app is a singleton. If the app is already running,
%% simply foreground it. We will never need multiple copies.
start() ->
	case whereis(?MODULE) of
		undefined -> do_start();
		PID -> wm:focus(PID)
	end.


%% Start a new instance of the app. Immediately takes focus.
do_start() ->
	Win = wm_window:start(?NAME),
	wm:app(
		PID = spawn(fun() -> start_server(Win) end),
		Win, ?NAME),
	os_debug:log(3,
		"Started window ~p for window switcher.",
		[Win]),
	% Register the process, such that multiple versions will not be
	% opened. If one is already running, calls to start/0 will simply
	% foreground it.
	register(?MODULE, PID),
	PID.

%% The server

%% Render the window for the first time, then start the server.
start_server(Win) ->
	server(refresh(#state { window = Win })).

server({stop, S}) ->
	% A stop has been requested. Remove the app from the WM and
	% free the window buffer.
	wm:remove_app(self()),
	wm_window:stop(S#state.window),
	ok;
server(S) ->
	receive
		% If we get a number key, calculate the actual number (non-ASCII).
		% Set the focus on this new window, then remove ourselves from the
		% app list and die quietly.
		{key, Char} when ((Char >= $0) and (Char =< $9)) ->
			server(switch_to_window(S, Char));
		{key, $n} ->
			app_console:start("Untitled"),
			server({stop, S});
		%% Quietly ignore other keys
		{key, _} -> server(S);
		X ->
			os_debug:log(
				warning, "Window switcher got unknown message ~p.", [X]),
			server(S)
	% Every half second, we should re-render the menu
	after ?REFRESH_TIME -> server(refresh(S))
	end.

%% Helper functions

%% Redraw the window and refresh the app list
refresh(S) ->
	os_debug:log(3, "Refresh called with S = ~p", [S]),
	render(
		S#state {
			app_list =
				sort_apps(
					[
						App
					||
						App <- wm:list_apps(),
						App#app.app =/= self()
					]
				)
		}
	).

%% Take an ASCII code for a digit and set the window accordingly
switch_to_window(S, Char) ->
	Num = Char - $0,
	if Num =< length(S#state.app_list) ->
		App = lists:nth(Num, S#state.app_list),
		os_debug:log(4, "Switching to window ~p (app ~p).",
			[Num, App#app.app]),
		wm:focus(App#app.app),
		{stop, S};
	true -> S
	end.

%% Sort the windows into alphabetical order
sort_apps(Apps) ->
	lists:sort(fun(A, B) -> A#app.name < B#app.name end, Apps).

%% Clear the screen and print the updated list of running apps
render(S) ->
	os_debug:log(3, "Rendering window list with app list of length ~p.",
		[length(S#state.app_list)]),
	% Clear the screen
	wm_window:render(S#state.window, rect,
		[
			0, 0, 80, 25, black, filled
		]
	),
	% Draw the title
	wm_window:render(S#state.window, print_str,
		[
			0, 0, "SWITCH TO A WINDOW"
		]
	),
	% Draw each row
	lists:foreach(
		fun(X) -> render_line(S, X) end,
		os_util:number(S#state.app_list)
	),
	% Detail the new console functionality.
	wm_window:render(S#state.window, print_str,
		[
			4, 23, "Press 'n' to start a new console."
		]
	),
	S.

%% Render a single app onto the output, starting on the second line.
render_line(S, {N, App}) ->
	wm_window:render(S#state.window, print_str,
		[
			0, N + 1, build_line(N, App)
		]
	).

%% Returns a string (which will become the row in our table)
%% for a given app.
build_line(N, A) ->
	pad(integer_to_list(N), 5) ++
		pad(format("~p", [A#app.app]), 10) ++
		pad(format("~p", [A#app.window]), 10) ++
		pad(format("~p", [A#app.name]), 40).

%%% HELPER FUNCTIONS

format(Fmt, Args) ->
	lists:flatten(io_lib:format(Fmt, Args)).

%% Pad a line to the correct size, or remove the last few letters
%% and replace them with an ellipses.
pad(Str, N) when length(Str) > N ->
	string:sub_string(Str, 1, N - 3) ++ "...";
pad(Str, N) ->
	string:left(Str, N, $ ).
