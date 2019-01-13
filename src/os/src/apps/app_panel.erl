-module(app_panel).
-export([start/1, start/2, start/3, start/4, stop/1]).

%%% A basic `panel' application that attaches to an output window,
%%% and displays it. The application can then be terminated by
%%% pressing escape.
%%%
%%% The panel can optionally be started with a number of functions
%%% that can be triggered by key press. These functions can also
%%% take an argument which can store an app state between trigger
%%% calls. This extra data can also be given a default value on 
%%% start.

-record(state, {
	window,
	triggers,
	extra
}).

%% The name of the application that should be given to the WM.
-define(DEFAULT_NAME, "Panel Output").

%% Start the application. If no name is provided, use the
%% default one. Assume no triggers unless informed otherwise.
start(Window) -> start(?DEFAULT_NAME, Window).
start(Name, Window) -> start(Name, Window, []).
start(Name, Window, Triggers) ->
	start(Name, Window, Triggers, undefined).
start(Name, Window, Triggers, ExtraData) ->
	ok =
		wm:app(
			PID = spawn(
				fun() ->
					server(
						#state {
							window = Window,
							triggers = Triggers,
							extra = ExtraData
						}
					)
				end),
			Window,
			Name
		),
	os_debug:log(5, "Started a panel (~p) connected to window ~p.",
		[Name, Window]),
	PID.

%% Shutdown the app, but don't kill the output window
stop(PID) ->
	PID ! stop,
	ok.

%% The only action the panel may take is terminating itself,
%% or executing a trigger
server(S) ->
	receive
		% If a pressed key matches a trigger, execute it, then
		% listen for more keys.
		{key, Key} ->
			os_debug:log(2, "app_panel got key: ~p.", [Key]),
			server(
				case lists:keyfind(Key, 1, S#state.triggers) of
					{Key, Fun} -> execute_trigger(S, Fun);
					false -> S
				end
			);
		% If someone asks us to, stop.
		stop -> do_stop(S)
	end.

%% Execute a trigger, optionally passing the panel state and
%% sotring a new one.
execute_trigger(S, Fun) ->
	case erlang:fun_info(Fun, arity) of
		{arity, 0} -> Fun(), S;
		{arity, 1} -> Fun(S)
	end.

%% Stop the panel, but do not kill the output window.
do_stop(_S) ->
	os_debug:log(3, "app_panel stopping."),
	wm:remove_app(self()),
	ok.
