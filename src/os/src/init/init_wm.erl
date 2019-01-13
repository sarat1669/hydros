-module(init_wm).
-export([start/0, requires/0]).

start() ->
	wm:start(),
	%Splash = app_splash_screen:start(self()),
	%receive {stopped, Splash} -> ok end,
	ok.

requires() -> [ init_keyboard, init_time, init_terminal ].

