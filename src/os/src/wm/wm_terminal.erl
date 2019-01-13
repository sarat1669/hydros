-module(wm_terminal).
-export([start/0, start/2, start/3, stop/0, stop/1]).
-export([start_default/0, get_default/0]).
-export([print_string/1, print_string/2, print_string/3]).
-export([set_focused/1, set_focused/2]).
-export([get_cursor/0, get_cursor/1, move_cursor/2, move_cursor/3]).
-export([scroll_forward/1, scroll_back/1]).
-export([set_contents/2]).
-export([blit/0, blit/1]).

%%% This module defines a terminal output and input program.
%%% Once started, this terminal can be written onto, and blitted
%%% onto the actual text console.
%%%
%%% All functions in this module (bar start and friends) default
%%% to sending the message to the system's default output. Optionally,
%%% the user can spawn more terminals and choose to print messages to
%%% those. For example, the user may want to create a new terminal
%%% specifically for executing one program in.

%%% An wm_terminal implements most of the interface of a wm_window,
%%% so it can often be used in place as an alternative.

%% Constants
-define(DEFAULT_HEIGHT, 25).
-define(DEFAULT_WIDTH, 80).

%% Defaults
-define(DEFAULT_CHAR, $  ).
-define(DEFAULT_FG, white).
-define(DEFAULT_BG, black).
-define(DEFAULT_NAME, "Default Terminal").
-define(DEFAULT_HISTORY_SZ, 500).

%% Record access macros.
%% The state record should always be in a variable called 'S'
%% in order for these macros to work. I know this looks disgusting,
%% but the code is vastly more readable this way, because otherwise
%% the record access code takes up too much space.
-define(WINDOW, S#state.window).
-define(X, S#state.cursor_x).
-define(Y, S#state.cursor_y).
-define(FG, S#state.fg_colour).
-define(BG, S#state.bg_colour).
-define(WIDTH, S#state.width).
-define(HEIGHT, S#state.height).
-define(SCROLL_BACK, S#state.scroll_back).
-define(SCROLL_FORWARD, S#state.scroll_forward).

%% State record definition

-record(state, {
	width,
	height,
	window, % Stores the associated window PID
	cursor_x = 0,
	cursor_y = 0,
	fg_colour = ?DEFAULT_FG,
	bg_colour = ?DEFAULT_BG,
	scroll_back = [],
	scroll_forward = []
}).

%%% PUBLIC INTERFACE FUNCTIONS

%% Start a new terminal output window. 
start() -> start(?DEFAULT_WIDTH, ?DEFAULT_HEIGHT).
start(Width, Height) -> start(Width, Height, ?DEFAULT_NAME).
start(Width, Height, Name) ->
	os_debug:log(3,
		"Starting a new wm_window. Width: ~p, height: ~p, name: ~p.",
		[Width, Height, Name]
	),
	spawn(
		fun() ->
			start_server(
				#state {
					width = Width,
					height = Height,
					window = wm_window:start(Width, Height, Name)
				}
			)
		end
	).

%% Start the default output terminal, as well as the log terminal.
%% Store the state of the window at startup as the start of the
%% log terminal contents.
start_default() ->
	Bin = drv_terminal:read(),
	register(?MODULE, start()),
	register(os_logs,
		LogTerm =
			start(?DEFAULT_WIDTH, ?DEFAULT_HEIGHT, "OS Log Terminal")),
	set_contents(LogTerm, Bin),
	ok.

%% Stop the default output. Probably a bit of a dangerous idea...
%% This will mean that all the collected output in the system
%% will be lost, if not sent directly to a specific terminal.
stop() -> stop(?MODULE).
%% Stops a terminal, being sure to also stop the window process.
stop(PID) ->
	PID ! stop,
	ok.

%% Return the default terminal.
get_default() ->
	whereis(?MODULE).

%% Set the contents of the buffer, in totality.
set_contents(Term, Bin) ->
	Term ! {set_contents, Bin},
	ok.

%% Print a formatted string onto the output
print_string(Str) ->
	print_string(?MODULE, Str).
print_string(Str, Opt) when is_list(Str) and is_atom(Opt) ->
	print_string(?MODULE, Str, Opt);
print_string(PID, Str) ->
	print_string(PID, Str, []).
print_string(PID, Str, Opt) when not is_list(Opt) ->
	print_string(PID, Str, [Opt]);
print_string(PID, Str, Opts) ->
	PID ! {print_string, Str, Opts},
	ok.

%% Paint the window onto the terminal output
blit() -> blit(?MODULE).
blit(PID) ->
	PID ! blit,
	ok.

%% Scroll the output forward
scroll_forward(PID) ->
	PID ! scroll_forward,
	ok.

%% Scroll the output backward
scroll_back(PID) ->
	PID ! scroll_back,
	ok.

%% Set the console to be 'focused'. This means that the window will be
%% automatically blitted when it is written to.
set_focused(State) -> set_focused(?MODULE, State).
set_focused(PID, State) ->
	PID ! {set_focused, self(), State},
	receive
		{focused, State} -> ok
	end.

%% Get cursor co-ordinates.
get_cursor() -> get_cursor(?MODULE).
get_cursor(PID) ->
	PID ! {get_cursor, self()},
	receive
		{cursor, X, Y} ->
			os_debug:log(1,
				"Got cursor location of [~w,~w] from terminal ~p.",
				[X, Y, PID]
			),
			{X, Y}
	end.

%% Set the position of the cursor, Optionally blitting it,
%% possibly saving the blitting for next time.
move_cursor(X, Y) -> move_cursor(get_default(), X, Y).
move_cursor(Terminal, X, Y) ->
	os_debug:log(1, "Moving cursor to [~w,~w] for terminal ~p.",
		[X, Y, Terminal]),
	Terminal ! {move_cursor, X, Y},
	ok.

%% SERVER

%% Draw a rect with the correct BG colour before the server starts.
start_server(S) ->
	os_debug:log(3, "Started terminal with window: ~p.", [?WINDOW]),
	wm_window:render(S#state.window, rect,
		[
			0, 0, ?WIDTH, ?HEIGHT, ?BG, filled
		]
	),
	os_debug:log(3, "Rendered first frame to ~p.", [?WINDOW]),
	server(S).

server(S) ->
	receive
		% Only get_cursor needs a response at the moment...
		{get_cursor, PID} ->
			PID ! {cursor, ?X, ?Y},
			server(S);
		% Asyncronous requests
		{print_string, Str, Opts} ->
			{Fun, FG, BG} = get_print_opts(S, Opts),
			server(Fun(S, Str, FG, BG));
		{set_focused, PID, State} ->
			wm_window:set_focused(?WINDOW, State),
			PID ! {focused, State},
			server(S);
		{move_cursor, X, Y} ->
			wm_window:move_cursor(?WINDOW, X, Y),
			server(S#state { cursor_x = X, cursor_y = Y });
		{set_contents, Bin} ->
			wm_window:set_contents(?WINDOW, Bin),
			server(
				do_print_string(
					S#state { cursor_x = 0, cursor_y = ?HEIGHT - 1 },
					"\n"
				)
			);
		scroll_forward ->
			server(do_scroll_forward(S));
		scroll_back ->
			server(do_scroll_back(S));
		blit ->
			os_debug:log(3, "Blitting terminal. Window: ~p.", [?WINDOW]),
			wm_window:blit(?WINDOW),
			server(S);
		% Safely terminate the server. This could leave us with no
		% terminal write errors to, so users must be careful.
		stop ->
			wm_window:stop(?WINDOW),
			ok
	end.

%%% PRIVATE HELPER FUNCTIONS

%% Extracts the function to call, foreground and background colours
%% from opts passed to print_string.
get_print_opts(S, Opts) ->
	{
		case lists:member(centered, Opts) of
			true -> fun do_print_centered/4;
			false -> fun do_print_string/4
		end,
		case lists:keyfind(fg, 1, Opts) of
			false -> ?FG;
			{fg, FG} -> FG
		end,
		case lists:keyfind(bg, 1, Opts) of
			false -> ?BG;
			{bg, BG} -> BG
		end
	}.

%% Print the string of characters on a new line, in the center
do_print_centered(S, Str, FG, BG) ->
	N = (?WIDTH div 2) - (length(Str) div 2),
	Line =
		spaces(N)
		++ Str
		++ spaces(?WIDTH - N - length(Str)),
	do_print_string(S, Line, FG, BG).

%% Print a string of characters onto the output.
%% Can accept deep IO lists!
do_print_string(S, Char) ->
	do_print_string(S, Char, ?FG, ?BG).
do_print_string(S, [], _FG, _BG) -> S;
do_print_string(S, [$\n|R], FG, BG) ->
	% Print a new line onto the output (by filling the current
	% line with spaces).
	do_print_string(
		S,
		spaces(?WIDTH - ?X) ++ R,
		FG, BG
	);
do_print_string(S, [$\t|R], FG, BG) ->
	% Print a tab character
	do_print_string(
		align_with_char(S, $ , 4),
		R, FG, BG
	);
do_print_string(S, [$\b|R], FG, BG) ->
	% Print a backspace character
	do_print_char(
		decrement_cursor_pos(S),
		$ , FG, BG
	),
	do_print_string(
		decrement_cursor_pos(S),
		R, FG, BG
	);
do_print_string(S, [Str|Rest], FG, BG) when is_list(Str) ->
	do_print_string(
		do_print_string(S, Str, FG, BG),
		Rest, FG, BG
	);
do_print_string(S, Str, FG, BG) ->
	% Print all the chars until the next special one.
	{Segment, Rest} = next_segment(Str),
	wm_window:render(
		S#state.window,
		print_str,
		[
			S#state.cursor_x,
			S#state.cursor_y,
			Segment,
			FG,
			BG
		]
	),
	do_print_string(increment_cursor_pos(S, length(Segment)), Rest, FG, BG).

safe_hd([]) -> empty_list;
safe_hd([H|_]) -> H.

safe_last([]) -> empty_list;
safe_last(L) -> lists:last(L).

%% Get all characters until char that needs special treatments.
% The string is built backwards and then reversed!
next_segment(Str) -> next_segment([], Str).
next_segment(Seg, []) -> {lists:reverse(Seg), []};
next_segment(Seg, Rest = [String|Rest]) when is_list(String) ->
	{lists:reverse(Seg), Rest};
next_segment(Seg, Str = [Char|Rest]) ->
	case lists:member(Char, [$\n, $\b, $\t]) of
		true -> {lists:reverse(Seg), Str};
		false -> next_segment([Char|Seg], Rest)
	end.

%% Print a single character onto the buffer and increment
%% the counters.
do_print_char(S, Char, FG, BG) ->
	wm_window:render(
		S#state.window,
		print,
		[
			S#state.cursor_x,
			S#state.cursor_y,
			Char,
			FG, BG
		]
	),
	increment_cursor_pos(S).

%% Incremement the position of the cursor, optionally scrolling
%% the console if required.
increment_cursor_pos(S) ->
	if ?X + 1 >= ?WIDTH ->
		increment_line_pos(S#state { cursor_x = 0 });
	true ->
		S#state { cursor_x = ?X + 1 }
	end.
% Increment the cursor N times
increment_cursor_pos(S, 0) -> S;
increment_cursor_pos(S, N) ->
	increment_cursor_pos(increment_cursor_pos(S), N - 1).

%% Increment the line position, optionally scrolling the output
%% as necessary.
increment_line_pos(S) ->
	if ?Y + 1 >= ?HEIGHT ->
		% We need to scroll the output buffer
		% First, let's read the line we are about to remove,
		% placing it on the scroll_back list
		NewS =
			S#state {
				scroll_back =
					[ wm_window:get_line(?WINDOW, 0) | ?SCROLL_BACK ]
			},
		wm_window:render(
			?WINDOW,
			scroll,
			[
				0,
				0,
				?WIDTH - 1,
				?HEIGHT - 1
			]
		),
		clear_line(NewS, NewHeight = ?HEIGHT - 1),
		NewS#state { cursor_y = NewHeight };
	true ->
		clear_line(S, NewHeight = ?Y + 1),
		S#state { cursor_y = NewHeight }
	end.

%% Scroll the terminal back one line, update the state
do_scroll_back(S = #state{ scroll_back = [] }) -> S;
do_scroll_back(S = #state { scroll_back = [Line|Rest] }) ->
	NewS = #state {
			scroll_forward = [wm_window:get_line(?WINDOW, ?HEIGHT)|?SCROLL_FORWARD],
			scroll_back = Rest
		},
	wm_window:render(
		?WINDOW,
		scroll_back,
		[0, 0, ?WIDTH - 1, ?HEIGHT - 1]
	),
	wm_window:put_line(?WINDOW, 0, Line),
	NewS.

%% Scroll the terminal down towards the bottom.
do_scroll_forward(_S) ->
	error(not_implemented).

%% Decrement the cursor position.
decrement_cursor_pos(S) ->
	if ?X - 1 < 0 ->
		S#state { cursor_x = ?WIDTH - 1, cursor_y = ?Y - 1 };
	true ->
		S#state { cursor_x = ?X - 1 }
	end.

%% Clear a line of the output
clear_line(S, Line) ->
	wm_window:render(
		?WINDOW,
		print_str,
		[0, Line, spaces(?WIDTH), ?FG, ?BG]
	).

%% Generate a string of n many spaces. Useful for clearing lines.
spaces(N) -> [ $  || _ <- lists:seq(1, N) ].

%% Continue to print char until atleast one has been printed
%% and the addr is divisible by N.
align_with_char(S, Char, N) -> align_with_char(S, [], Char, N).
align_with_char(S, Str, Char, N) ->
	case {?X div N, length(Str)} of
		{0, StrLen} when StrLen > 0 -> do_print_string(S, Str);
		_ -> align_with_char(S, [Char|Str], Char, N)
	end.
