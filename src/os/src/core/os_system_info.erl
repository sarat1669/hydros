-module(os_system_info).
-export([get_memory/0, get_proc_id/0, get_num_procs/0]).
-export([get_pit_counter/0, get_lapic_counter/0]).
-export([set_lapic_inc/1]).
-export([get_time/0, set_time/1]).
-export([is_first_boot/0]).

get_memory() ->
	% TODO: Actually get the memory amount
	16#20000000.

get_proc_id() -> os_unsafe:get_sys_val(id).
get_num_procs() -> os_unsafe:get_sys_val(procs).

get_pit_counter() -> os_unsafe:get_sys_val(pit_count).
get_lapic_counter() -> os_unsafe:get_sys_val(lapic_count).

% Set the number of microseconds to increase per LAPIC tick.
set_lapic_inc(X) ->
	os_unsafe:set_sys_val(lapic_inc, X).

get_time() -> os_unsafe:get_sys_val(time).
set_time(X) -> os_unsafe:set_sys_val(time, X).

is_first_boot() ->
	os_unsafe:get_sys_val(first_boot) == 1.
