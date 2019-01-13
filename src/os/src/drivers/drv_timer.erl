-module(drv_timer).
-export([start/0]).

%%% Configure and start the timer subsystem.

start() ->
	Start = os_system_info:get_lapic_counter(),
	drv_lapic:init_timer(16#C00),
	drv_pit:set_timer(100),
	os_debug:log("Enabling PIT."),
	drv_pit:enable(),
	os_unsafe:instruction(sti),
	os_debug:log("Waiting for PIT ticks."),
	os_util:until(
		fun() ->
			os_system_info:get_pit_counter() >= 100
		end
	),
	End = os_system_info:get_lapic_counter(),
	os_debug:log("Counted 100 ticks. Disabling PIT."),
	drv_pit:disable(),
	os_debug:log("Adjusting LAPIC timer settings."),
	os_system_info:set_time(0),
	os_system_info:set_lapic_inc(Period = 1000000 div (End - Start + 1)),
	os_debug:log("Set Local APIC timer tick to ~w us.", [Period]),
	ok.
