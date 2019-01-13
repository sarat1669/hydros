-module(app_logs).
-export([start/0]).

%%% A basic log viewer application. Built upon a basic app_panel.

%% The name of the application that should be given to the WM.
-define(DEFAULT_NAME, "Panel Output").

%% Ensure that the app is a singleton. If the app is already running,
%% simply foreground it. We will never need multiple copies.
start() ->
	case whereis(?MODULE) of
		undefined -> do_start();
		PID -> wm:focus(PID)
	end.

%% Start the application.
do_start() ->
	App =
		app_panel:start(
			"System Log Viewer",
			whereis(os_logs),
			[{esc, fun() -> app_panel:stop(self()) end}]
		),
	register(?MODULE, App),
	App.
