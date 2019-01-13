-module(app_presentation).
-export([start/1]).

%% The name of the app, as detailed in the app switcher, etc.
-define(NAME, "Presentation").

-record(state, {
	window,
	frame = 1,
	slides = [],
	stopping = false
}).

start(SlideMod) ->
	Win = wm_window:start(?NAME),
	wm:app(
		AppPID = spawn(fun() -> start_server(Win, SlideMod) end),
		Win, ?NAME),
	os_debug:log(5, "Starting presentation. Win: ~p.", [Win]),
	AppPID.

%% The server

%% Render the window for the first time, then start the server.
start_server(Win, SlideMod) ->
	server(render(#state { window = Win, slides = SlideMod:slides() })).

%% The main app server. Takes keyboard input and processes it.
server(#state { stopping = true, window = Win }) ->
	wm:remove_app(self()),
	wm_window:stop(Win),
	ok;
server(S) ->
	receive
		{key, Char} ->
			server(handle_input(S, Char))
	end.

%% Takes the current state and some input, processes it and returns
%% a new state.
handle_input(S, Char) ->
	render(
		case Char of
			left -> alter_frame(S, dec);
			right -> alter_frame(S, inc);
			esc -> S#state { stopping = true };
			_ -> S
		end
	).

%% Change the frame number, ensuring not to go over the length, or
%% under 1.
alter_frame(S = #state { frame = Frame, slides = Slides }, inc)
		when length(Slides) == Frame -> S;
alter_frame(S = #state { frame = 1 }, dec) -> S;
alter_frame(S = #state { frame = Frame }, inc) ->
	S#state { frame = Frame + 1 };
alter_frame(S = #state { frame = Frame }, dec) ->
	S#state { frame = Frame - 1 }.

%% Re-render the app's window, optionally updating state.
render(S) ->
	% Clear the screen
	wm_window:render(S#state.window, rect,
		[
			0, 0, 80, 25, black, filled
		]
	),
	display(S, lists:nth(S#state.frame, S#state.slides)),
	S.

%% Display a slide, after the screen is cleared.
display(S, Title) when is_list(Title) ->
	display_line_centered(S, Title);
display(S, {Title, Fun}) when is_function(Fun) ->
	display(S, {Title, [], Fun});
display(S, {Title, Points, Fun}) when is_function(Fun) ->
	case erlang:fun_info(Fun, arity) of
		{arity, 1} -> Fun(S#state.window);
		{arity, 0} -> Fun()
	end,
	display(S, {Title, Points});
display(S, {Title, Lines}) ->
	display_line_centered(S, 0, Title),
	display_line_centered(S, 1, [ 205 || _ <- lists:seq(1, 10) ]),
	lists:foreach(
		fun({LineNum, Line}) ->
			display_line(S, LineNum + 2, Line)
		end,
		os_util:number(calculate_centred_offset(Lines, 25), Lines)
	).

%% Display a line of the input.
display_line(S, Line, "-" ++ Str) ->
	display_line(S, Line, [175|Str]);
display_line(S, Line, Str) ->
	wm_window:render(
		S#state.window,
		print_str,
		[
			0,
			Line,
			Str
		]
	).

%% Write the title of the talk in the centre of the screen.
display_line_centered(S, Str) ->
	display_line_centered(S, 12, Str).
display_line_centered(S, Line, Str) ->
	wm_window:render(
		S#state.window,
		print_str,
		[
			calculate_centred_offset(Str, 80),
			Line,
			Str
		]
	).

%% Calculate the 'centred' position of a list.
calculate_centred_offset(List, Size) ->
	(Size div 2) - (length(List) div 2).
