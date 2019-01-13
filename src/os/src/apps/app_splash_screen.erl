-module(app_splash_screen).
-export([start/0, start/1, stop/1]).

%%% A basic splash screen displaying application. I might generalise it later,
%%% so we can have general splash screens, but I can't imagine how useful that
%%% would be? How many HydrOS games are we expecting there to be?
%%% None, I imagine.

%% Define the name of the window and application
-define(NAME, "HydrOS Splash Screen").
%% Number of frames per second.
-define(REFRESH_RATE, 2).
%% Number of frames per second.
-define(FRAMES, 6).
%% The number of characters per line.
-define(WIDTH, 80).
%% The number of lines per screen.
-define(HEIGHT, 25).

-record(state, {
	window,
	frame = 0,
	on_stop % A function that will exit on 
}).

%% Start a new instance of the splash screen. Immediately takes focus.
%% Optionally take a PID to inform when the frames have all been rendered,
%% It can also take a function to execute on exit.
start() -> start(fun(_S) -> do_nothing end).
start(PID) when is_pid(PID) -> start(fun(S) -> on_exit_inform(S, PID) end);
start(Fun) ->
	Win = wm_window:start(?WIDTH, ?HEIGHT, ?NAME),
	wm:app(
		AppPID = spawn(fun() -> start_server(Win, Fun) end),
		Win, ?NAME),
	os_debug:log(5, "Started splash screen. Win: ~p.", [Win]),
	AppPID.

%% Cause a spalshscreen to terminate early.
stop(PID) ->
	PID ! stop,
	ok.

%% The server

%% Render the window for the first time, then start the server.
start_server(Win, Fun) ->
	server(render(#state { window = Win, on_stop = Fun })).

server({stop, S}) -> do_stop(S);
server(S) ->
	receive
		{key, _} -> server(S);
		stop -> server({stop, S})
	% Every half second, we should re-render the menu
	after 1000 div ?REFRESH_RATE -> server(render(S))
	end.

%% Helper functions

%% Clear the screen and print the correct logo frame
render(S = #state { frame = ?FRAMES }) ->
	os_debug:log(5, "Splash screen reached final frame, ~p.", [?FRAMES + 1]),
	{stop, S};
render(S) ->
	os_debug:log(3, "Rendering frame ~p.", [S#state.frame]),
	{TextFG, TextBG, BG} = frame(S),
	% Clear the screen
	wm_window:render(S#state.window, rect,
		[
			0, 0, ?WIDTH, ?HEIGHT, BG, filled
		]
	),
	%% Draw the logo, line by line.
	StartLine = calculate_centred_offset(Logo = logo(S), ?HEIGHT),
	lists:foreach(
		fun({Line, Str}) ->
			wm_window:render(
				S#state.window,
				print_str,
				[calculate_centred_offset(Str, ?WIDTH), Line, Str, TextFG, TextBG]
			)
		end,
		os_util:number(StartLine, Logo)
	),
	S#state { frame = S#state.frame + 1 }.

%% Calculate where to start printing a list of things, such that the body of the 
%% content is in the center of the available space. This is generalised such that
%% it will function correctly for strings and lists of strings.
calculate_centred_offset(List, Size) ->
	(Size div 2) - (length(List) div 2).

%% Informa pid that the splash screen has stopped. This is the default on_stop
%% action, if a PID is specified to start().
on_exit_inform(_S, PID) ->
	os_debug:log(3, "Informing ~p that splash screen has finished.", [PID]),
	PID ! {stopped, self()},
	ok.

%% Stop the server, being sure to kill the output window, and unregister
%% with the WM. We also execute the on_exit function here.
do_stop(S) ->
	os_debug:log(5,
		"Splash screen server stopping. Dispatching on_exit.", []),
	% Call the on_stop callback
	(S#state.on_stop)(S),
	% Remove ourselves from the WM
	wm:remove_app(self()),
	% Free our window buffer
	wm_window:stop(S#state.window),
	ok.

%% Returns a set of decoration instructions for drawing the logo, per frame.
%% Output format is: {TextForeground, TextBackground, Background}.
frame(S) when is_record(S, state) -> frame(S#state.frame);
frame(0) ->		{black, black, black};
frame(1) ->		{dark_grey, black, black};
frame(2) ->		{light_grey, black, black};
frame(3) ->		{blue, black, black};
frame(4) ->		{light_blue, black, black};
frame(5) ->		{cyan, black, black}.

%% The system logo, with no newlines. Can be printed as-is on an 80 width terminal.
logo(_S) ->
	[
	"88        88                      88              ,ad8888ba,    ad88888ba ",
	"88        88                      88             d8'`    `'8b  d8'     `8b",
	"88        88                      88            d8'        `8b Y8,        ",
	"88aaaaaaaa88 8b       d8  ,adPPYb,88 8b,dPPYba, 88          88 `Y8aaaaa,  ",
	"88''''''''88 `8b     d8' a8`    `Y88 88P'   `Y8 88          88   `'''''8b,",
	"88        88  `8b   d8'  8b       88 88         Y8,        ,8P         `8b",
	"88        88   `8b,d8'   '8a,   ,d88 88          Y8a.    .a8P  Y8a     a8P",
	"88        88     Y88'     `'8bbdP'Y8 88           `'Y8888Y''    'Y88888P' ",
	"                 d8'                                                      ",
	"                d8'                                                       "
	].
