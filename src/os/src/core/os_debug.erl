-module(os_debug).
-export([log/1, log/2, log/3]).
-export([bochs_console_write/1, bochs_console_write_char/1, break/0, rdtsc/0]).
-export([hang/0, shutdown/0, restart/0]).
-export([p/1, p/2, print_obj/1, print_obj/2]).

%%% A module that contains a variety of simple tools to help with
%%% the process of debugging HydrOS. HydrOS developers are encouraged
%%% to place calls to os_debug:log/2-3 throughout the code, in useful
%%% places. Log levels are defined roughly as follows:
%%% 1: The least important or useful logs. Presence checks. 'LINE X', etc.
%%% 2: More useful logs that generate large amounts of information.
%%% 3: Averagely useful logs that don't print too much information.
%%% 4: A summary of received user input before processesing.
%%% 5: A summary of computation, before being sent to the user.

%% Calls to os_debug:log/2/3 will only print if they're level is
%% equal or higher than this.
-define(LOG_LEVEL, 1).
%% The default log level, for calls that do not specify a level.
-define(LOG_DEFAULT_LEVEL, 2).
%% How should unknown level symbols be interpreted?
-define(UNKNOWN_TAG_LEVEL, warning).
%% Defines the warning level that various symbols are interpreted as.
%% We also specify here what prefix should be printed for a given sym.
-define(LOG_SYMBOLS,
	[
	 	{info, 3, "INFO"},
		{warning, 4, "WARN"},
		{error, 5, "ERR"},
		{important, 10, "IMPORTANT"},
		{debug, 10, "DEBUG"}
	]
).

%% A basic 'log and print' function.
p(X) -> print_obj(X).
p(Str, Xs) -> print_obj(Str, Xs).

%% Print an object to the standard terminaxl
print_obj(X) -> print_obj("~p", [X]).
print_obj(Str, X) when not is_list(X) -> print_obj(Str, [X]);
print_obj(Str, Xs) ->
	io:format(
		"[Debug]{~p} " ++ Str ++ "~n.",
		[self()|Xs]
	).

%% Log a string (if it is >= LOG_LEVEL) to the BOCHS output.
log(Str) -> log(?LOG_DEFAULT_LEVEL, Str, []).
log(Fmt, Args) when is_list(Fmt) -> log(?LOG_DEFAULT_LEVEL, Fmt, Args);
log(Level, Fmt) when is_integer(Level) -> log(Level, Fmt, []);
log(Level, Fmt) when is_atom(Level) -> log(Level, Fmt, []).
log(Level, _, _) when Level < ?LOG_LEVEL -> ok;
log(Symbol, Fmt, Args) when is_atom(Symbol) ->
	case lists:keyfind(Symbol, 1, ?LOG_SYMBOLS) of
		false ->
			log(?UNKNOWN_TAG_LEVEL,
				"The log message following this one is of unknown symbol ~p. "
					"Processing as ~p.",
				[Symbol, ?UNKNOWN_TAG_LEVEL]
			),
			log(?UNKNOWN_TAG_LEVEL, Fmt, Args);
		{Symbol, Level, Prefix} ->
			log(Level, Prefix ++ ": " ++ Fmt, Args)
	end;
log(Level, Fmt, Args) ->
	Tag =
		io_lib:format(
			"[~p|~p] ",
			[
				case Level of {_, Sym} -> Sym; _ -> Level end,
				self()
			]
		),
	String = io_lib:format(Fmt, Args),
	bochs_console_write(Output = Tag ++ String ++ "\n"),
	write_to_log(Tag, String),
	Output.

%% Log a string to the os_log shared terminal, checking first
%% that the log has been initialised.
write_to_log(Tag, Str) ->
	case whereis(os_logs) of
		undefined -> ok;
		PID ->
			io:format(PID, "~s", [Tag], [{fg, red}]),
			io:format(PID, "~s~n", [Str])
	end.

%% Write a string onto the bochs debugging console.
%% Can accept deep lists.
bochs_console_write([]) -> ok;
bochs_console_write([Str|Rest]) when is_list(Str) ->
	bochs_console_write(Str),
	bochs_console_write(Rest);
bochs_console_write([C|Str]) ->
	bochs_console_write_char(C),
	bochs_console_write(Str).

%% Write a character onto the host machine's terminal.
bochs_console_write_char(C) ->
	os_unsafe:port_out(16#e9, C).

%% Trigger a BOCHS breakpoint.
break() -> os_unsafe:instruction(break).

%% Read the timestamp counter
rdtsc() ->
	{High, Low} = os_unsafe:instruction(rdtsc),
	(High bsl 32) bor Low.

%% Shutdown the machine, assuming we are running in QEMU or Bochs.
%% Does not work on real machines.
shutdown() ->
	drv_port:out(16#f4, 0),
	lists:foreach(
		fun(Char) -> drv_port:out(16#8900, Char) end,
		"Shutdown"
	).

%% Restart a machine using the keyboard to trigger the CPU's reset line.
%% Why on earth is x86 setup this way? Why should my keyboard be able to
%% do this?
restart() -> restart(2).
restart(X) when (X band 2) =/= 0 -> restart(drv_port:in(16#64));
restart(_) -> drv_port:out(16#64, 16#FE).

%% Hang the machine in an uninterruptable way. Not a busy wait.
hang() ->
	os_unsafe:instruction(sti),
	os_unsafe:instruction(hlt).
