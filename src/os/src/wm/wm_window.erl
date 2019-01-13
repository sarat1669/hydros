-module(wm_window).
-export([start/0, start/1, start/2, start/3, stop/1]).
-export([get_buffer/1, get_name/1, get_line/2, put_line/3]).
-export([blit/1, render/3, render_batch/2, move_cursor/3]).
-export([set_contents/2]).
-export([set_focused/2]).

%%% Tracks and handles the drawing of a fullscreen window. A wm_window
%%% process manages a window buffer, and is the sole writer. These processes
%%% also control when the window is blitted to the screen.

-define(DEFAULT_WIDTH, 80).
-define(DEFAULT_HEIGHT, 25).
-define(DEFAULT_SZ, ?DEFAULT_WIDTH * ?DEFAULT_HEIGHT * 2).

% The maximum number of render requests to perform without blitting.
-define(MAX_RENDER_BATCH_SZ, 25).

-record(state, {
	name = noname, % The name of window, will be shown in task manager apps
	% Alloc buffer on record create
	addr = os_unsafe:malloc(?DEFAULT_SZ * 8), 
	rows = ?DEFAULT_HEIGHT,
	cols = ?DEFAULT_WIDTH,
	renderings = 0, % A count of the calls to render/3
	last_blit = 0, % The render number of the last time we were blitted
	focused = false,
	cursor_x = ?DEFAULT_WIDTH - 1,
	cursor_y = ?DEFAULT_HEIGHT - 1
}).

%%% PUBLIC API

%% Starts a window server. Returns the new PID.
start() -> start(?DEFAULT_WIDTH, ?DEFAULT_HEIGHT).
start(Name) -> start(?DEFAULT_WIDTH, ?DEFAULT_HEIGHT, Name).
start(Cols, Rows) -> start(Cols, Rows, noname).
start(Cols, Rows, Name) ->
	spawn(
		fun() ->
			server(
				#state{
					name = Name,
					cols = Cols,
					rows = Rows
				}
			)
		end
	).

%% Stop a window and be sure to free the buffer!
stop(PID) ->
	PID ! stop.

%% Set the contents of the buffer, in totality.
set_contents(Win, Bin) ->
	Win ! {set_contents, Bin},
	ok.

%% Returns the name of the window process requested
get_name(PID) ->
	PID ! {get_name, self()},
	receive
		{name, N} -> N
	end.

%% Returns a pointer reference to the buffer held by
%% the window process.
get_buffer(PID) ->
	PID ! {get_buffer, self()},
	receive
		{buffer, B} -> B
	end.

%% Returns the data held about a given line of the output.
%% Takes a line number and returns the data associated with it
get_line(PID, Num) ->
	PID ! {get_line, self(), Num},
	receive
		{line, Num, Bin} -> Bin
	end.

%% Write a binary in position for a line.
put_line(PID, Num, Bin) ->
	PID ! {put_line, Num, Bin},
	ok.

%% Print the window to the terminal output.
blit(PID) ->
	PID ! blit,
	ok.

%% Move the cursor to a given location, when the data
%% is being blitted.
move_cursor(PID, X, Y) ->
	PID ! {set_cursor, X, Y},
	ok.

%% Takes the name of a function from drv_terminal and some
%% arguments, then renders the request on the wm_window.
%% Drawing is done on the server, asyncronously.
render(PID, Fun, Args) ->
	PID ! {render, Fun, Args},
	ok.

%% Renders a batch of instructions at once, blitting only once.
%% Takes a list of tuples of function name and arguments.
%% 		[{FunctionName, Arguments}|...]
render_batch(PID, Batch) ->
	PID ! {render_batch, lists:flatten(Batch)},
	ok.

%% Set whether we should automatically blit the window to the
%% terminal, or not.
set_focused(PID, State) ->
	PID ! {set_focused, self(), State},
	receive
		{focused, State} -> ok
	end.

%%% SERVER AND BACKEND HELPERS

%% Services render and blit calls from the window-owning app.
server(S) ->
	receive
		{get_buffer, PID} ->
			PID ! {buffer, S#state.addr},
			server(S);
		{get_name, PID} ->
			PID ! {name, S#state.name},
			server(S);
		{get_line, PID, Num} ->
			PID ! {line, Num, drv_terminal:get_line(S#state.addr, Num)},
			server(S);
		{put_line, Num, Bin} ->
			drv_terminal:put_line(S#state.addr, Num, Bin),
			server(S);
		{set_contents, Bin} ->
			os_debug:log(3, "Setting contents of window to binary of size ~p.",
				[size(Bin)]),
			drv_terminal:blit(S#state.addr, Bin),
			server(optional_blit(S));
		{set_cursor, X, Y} ->
			server(optional_blit(S#state { cursor_x = X, cursor_y = Y }));
		{set_focused, PID, X} ->
			os_debug:log(2, "Setting focused: ~p.", [X]),
			NewS = optional_blit(do_set_focused(S, X)),
			PID ! {focused, NewS#state.focused},
			server(NewS);
		{render, Fun, Args} ->
			server(do_render_batch(S, get_render_batch(Fun, Args)));
		{render_batch, Batch} ->
			server(do_render_batch(S, Batch));
		blit ->
			os_debug:log(3, "Raw blit called!"),
			server(do_blit(S));
		stop ->
			os_unsafe:free(S#state.addr),
			ok
	end.

%% Gets all render requests from the message queue.
%% NOTE: The list is built in reverse for efficiencies sake.
%% It is then reversed.
get_render_batch(Fun, Args) when is_atom(Fun) and is_list(Args) ->
	get_render_batch(1, [{Fun, Args}]);
get_render_batch(N = ?MAX_RENDER_BATCH_SZ, Requests) ->
	os_debug:log(warning, "Produced a batch of the maximum size (~p).", [N]),
	lists:reverse(Requests);
get_render_batch(N, Requests) ->
	receive
		{render, Fun, Args} ->
			get_render_batch(N + 1, [{Fun, Args}|Requests])
	after 0 ->
		lists:reverse(Requests)
	end.

%% Set the server state focused value
do_set_focused(S, X) -> S#state { focused = X }.

%% Blit to the screen ONLY IF this window is focused.
%% Apply all of the focus messages in the queue beforehand.
optional_blit(S) ->
	receive
		{set_focused, X} -> optional_blit(do_set_focused(S, X))
	after 0 ->
		case S#state.focused of
			true -> do_blit(S);
			false -> S
		end
	end.

%% Blit the window, update the counter
do_blit(
	S = #state {
		renderings = Renderings,
		cursor_x = X,
		cursor_y = Y
	}) ->
	drv_terminal:blit(S#state.addr),
	drv_cursor:move_to(X, Y),
	S#state { last_blit = Renderings }.

%% 'Execute' a render batch, then do one blit at the end.
do_render_batch(S, []) -> optional_blit(S);
do_render_batch(S, [{Fun, Args}|Rest]) ->
	do_render_batch(
		do_render(S, Fun, Args),
		Rest
	).

%% Apply a rendering call to the buffer.
do_render(S = #state { renderings = Renderings }, Fun, Args) ->
	apply(
		drv_terminal,
		Fun,
		[ S#state.addr | Args ]
	),
	S#state { renderings = Renderings + 1 }.
