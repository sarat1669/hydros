-module(app_console).
-export([start/0, start/1, start_default/0]).
-export([stop/0, stop/1]).

%%% A basic REPL for the OS.
%%% Listens to the keyboard for input, reads it into a string until it receives
%%% an enter. Once enter has been received, it parses the string and executes
%%% it with the same bindings as the previous command.

-record(state, {
	terminal, % The terminal we should be printing to
	string = "",
	bindings = erl_eval:new_bindings(),
	name
}).

%% The name of the application that should be given to the WM.
-define(DEFAULT_NAME, "Default").

%% Defines the set of shortcuts accepted by the console.
%% This is a key value pair where the key is the literal keyboard
%% character that the server receives, and the value is the string
%% that should be executed.
%%
%% NOTE: These commands should not be terminated by a '.'
-define(SHORTCUTS,
	[
		{f1, "Show all shortcuts."},
		{f2, "erlang:get_stacktrace()"},
		{f3, "init_pingpong:start()"},
		{f4, "app_presentation:start(prs_nutshell)"},
		{f5, "os_test:port_ok()"},
		{f6, "os_test:port_fail()"},
		{f7, "os_ipc:cast(1, fun os_unsafe:interrupt/0)"}
	]
).

%% A macro that gives us fast access to the terminal data
%% and in the state record.
-define(TERM, S#state.terminal).

%% Start a new console. By default, print to the default
%% terminal. Optionally, takes another terminal to print
%% to.
start() -> start(?DEFAULT_NAME).
start(Name) ->
	start(Name, wm_terminal:start()).
start(Name, Terminal) ->
	ok =
		wm:app(
			PID = spawn(fun() -> start_server(Name, Terminal) end),
			Terminal,
			Name
		),
	os_debug:log(5, "Started app_console ~p with terminal ~p.",
		[PID, Terminal]),
	PID.

%% Start the default registered console, with the default terminal.
start_default() ->
	register(?MODULE, start(?DEFAULT_NAME, wm_terminal:get_default())).

%% Stop a given console.
stop() -> stop(?MODULE).
stop(PID) ->
	PID ! stop,
	ok.

%% Prepare the zerver for first run.
start_server(Name, Terminal) ->
	wm_terminal:print_string(
		Terminal, "HYDROS CONSOLE: " ++ Name),
	% TODO: Potentially, print HydrOS version info.
	io:format(Terminal,
		"~n~nPress F1 at any time to see a list of shortcuts.~n"),
	new_prompt(S = #state{ name = Name, terminal = Terminal }),
	set_cursor(S),
	os_debug:log(3, "Console init done."),
	server(S).

%% When a key is received, that key should simply be processed,
%% including printing results, error and newlines, then the cursor
%% code checks whether the cursor should be moved, and then we listen
%% for another key.
server(S) ->
	receive
		% TODO: fix terminal scrolling and re-enable these.
%		{key, up} ->
%			wm_terminal:scroll_back(S#state.terminal),
%			server(S);
%		{key, down} ->
%			wm_terminal:scroll_back(S#state.terminal),
%			server(S);
		{key, Key} ->
			% Handle a key press, including shortcut execution.
			case lists:keyfind(Key, 1, ?SHORTCUTS) of
				false ->
					% The key is not a shortcut. Handle it normally.
					server(set_cursor(handle_key(S, Key)));
				{Key, String} ->
					% The user has requested that we execute a shortcut
					handle_shortcut(S, Key, String)
			end;
		stop ->
			wm:remove_app(self()),
			wm_terminal:stop(?TERM),
			ok
	end.

%% Run shortcuts
% Handle special shortcuts first, then the general case.
handle_shortcut(S, f1, _) ->
	io:format(?TERM, "~n"),
	show_shortcuts(S),
	new_prompt(S),
	server(S);
handle_shortcut(S, Key, Command) ->
	io:format(?TERM, "~nCONSOLE: Executing shortcut ~p ('~s')...~n",
		[Key, Command]),
	server(
		set_cursor(
			handle_key(
				S#state { string = Command ++ "." },
				enter
			)
		)
	).

%% Execute the command on enter
handle_key(S = #state { string = Str, bindings = Bnds }, enter) ->
	S#state { string = "", bindings = execute_string(S, Str, Bnds) };
%% Remove the last character on backspace.
handle_key(S = #state { string = Str }, bksp) when length(Str) > 0 ->
	io:format(?TERM, "\b"),
	S#state {
		string = lists:reverse(tl(lists:reverse(Str)))
	};
%% Ignore other control characters.
handle_key(S, X) when is_atom(X) -> S;
%% Print any non-control characters and add the character to the command.
handle_key(S = #state { string = Str }, Char) ->
	io:format(?TERM, "~s", [[Char]]),
	S#state { string = Str ++ [Char] }.

%% Takes an input string and attempts to manage it's execution and the
%% expression of that execution to the user (for example, new lines, 
%% printing results and new prompts).
execute_string(S, "", Bnds) ->
	io:format(?TERM, "~n"),
	new_prompt(S),
	Bnds;
execute_string(S, Str, Bnds) ->
	os_debug:log(2, "Console has string '~s' so far.", [Str]),
	io:format(?TERM, "~n"),
	os_debug:log(5, "Execing string '~s'.", [Str]),
	try do_exec(Str, Bnds) of
		{Res, NewBnds} ->
			io:format(?TERM, "~p~n", [Res]),
			new_prompt(S),
			NewBnds
	catch
		Type:Details ->
			io:format(?TERM, "~p: ~p~n", [Type, Details]),
			new_prompt(S),
			Bnds
	end.

%% Try to execute the input string, using the Erlang compiler
%% infrastrucutre, as well as erl_eval:exprs/2.
do_exec(String, Bindings) ->
	{ok, Tokens, _} = erl_scan:string(String),
	ASTRes =  erl_parse:parse_exprs(Tokens),
	{ok, AST} = ASTRes,
	Result = erl_eval:exprs(AST, Bindings),
	{value, Res, NewBindings} = Result,
	{Res, NewBindings}.

%% Move the cursor of the terminal output, to where it should be
set_cursor(S) ->
	{X, Y} = wm_terminal:get_cursor(?TERM),
	wm_terminal:move_cursor(?TERM, X, Y),
	S.

%% Print a new prompt
new_prompt(S) ->
	io:format(?TERM, "[", [],  [{fg, green}]),
	io:format(?TERM, "P:~p", [self()], [{fg, red}]),
	io:format(?TERM, "]", [],  [{fg, green}]),
	io:format(?TERM, ":- ", [],  [{fg, magenta}]).

%% Print a list of the supported shortcuts to the default terminal.
show_shortcuts(S) ->
	io:format(?TERM, "Shortcuts supported by this console:~n"),
	lists:foreach(
		fun({Key, Command}) ->
			io:format(?TERM,
				"~p        '~s'~n",
				[Key, Command]
			)
		end,
		?SHORTCUTS
	).
