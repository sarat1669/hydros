-module(drv_pit).
-export([enable/0, disable/0, set_timer/1, one_shot/1, check_one_shot/0]).

-define(FREQ, 1193182).

%%% A module that supplies basic support for basic
%%% Programmable Interval Timer operation.

enable() -> drv_ioapic:enable_interrupt(2).

disable() -> drv_ioapic:disable_interrupt(2).

% Set the timer to interrupt X timers per second
set_timer(X) ->
	drv_port:out(16#43, 16#34), % Counter 0, rategen, 16bit
	drv_port:out(16#40, timer_div(X) rem 256),
	drv_port:out(16#40, timer_div(X) div 256).

timer_div(X) -> (?FREQ + X) div X.

one_shot(Div) ->
	drv_port:out(16#43, 16#30),
	drv_port:out(16#40, 16#A9),
	drv_port:out(16#40, Div).


check_one_shot() ->
	drv_port:out(16#43, 16#E2),
	(drv_port:in(16#40) band 64) =/= 0.


