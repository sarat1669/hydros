-module(os_unsafe).
-export([read/2, write/2, write/3]).
-export([lock/1, unlock/1]).
-export([malloc/1, free/1]).
-export([port_in/1, port_out/2]).
-export([port_in32/1, port_out32/2]).
-export([interrupt/0, get_sys_val/1, set_sys_val/2, instruction/1]).
-export([format/1, print_string/1]).
-export([hang/0]).
-export([load_pagetable/1]).

-include("cap.hrl").

%% Memory accessors.

read(Addr, Len) -> 
	validate_memory(Addr, Len),
	?MODULE:do_read(Addr, Len).

write(Addr, Bin) -> 
	validate_memory(Addr, byte_size(Bin)),
	?MODULE:do_write(Addr, Bin).

write(Addr, Bin, CopySize) -> 
	validate_memory(Addr, byte_size(Bin)),
	?MODULE:do_write(Addr, Bin, CopySize).

lock(Addr) ->
	validate_memory(Addr, 1),
	?MODULE:do_lock(Addr).

unlock(Addr) ->
	validate_memory(Addr, 1),
	?MODULE:do_unlock(Addr).

malloc(Amount) ->
	validate_system(),
	?MODULE:do_malloc(Amount).

free(Addr) ->
	validate_system(),
	?MODULE:do_free(Addr).

%% Port accessors.

port_in(Addr) ->
	validate_ports(Addr),
	?MODULE:do_port_in(Addr).

port_out(Addr, Data) ->
	validate_ports(Addr),
	?MODULE:do_port_out(Addr, Data).

port_in32(Addr) ->
	validate_ports(Addr),
	?MODULE:do_port_in32(Addr).

port_out32(Addr, Data) ->
	validate_ports(Addr),
	?MODULE:do_port_out32(Addr, Data).

%% System admin.

get_sys_val(Type) ->
	validate_system(),
	?MODULE:do_get_sys_val(Type).

set_sys_val(Type, Val) ->
	validate_system(),
	?MODULE:do_set_sys_val(Type, Val).

instruction(Name) ->
	validate_system(),
	?MODULE:do_instruction(Name).

interrupt() ->
	validate_system(),
	?MODULE:do_interrupt().

format(Str) ->
	validate_system(),
	?MODULE:do_format(Str).

print_string(Str) ->
	validate_system(),
	?MODULE:do_print_string(Str).

hang() -> ?MODULE:instruction(hang).

load_pagetable(_) -> erlang:nif_error(undefined).

%% Stubs

do_read(_Addr, _Len) -> erlang:nif_error(undefined).
do_write(_Addr, _Bin) -> erlang:nif_error(undefined).
do_write(_Addr, _Bin, _CopySize) -> erlang:nif_error(undefined).

do_lock(_Addr) -> erlang:nif_error(undefined).
do_unlock(_Addr) -> erlang:nif_error(undefined).

do_malloc(_Amount) -> erlang:nif_error(undefined).
do_free(_Amount) -> erlang:nif_error(undefined).

do_port_in(_Addr) -> erlang:nif_error(undefined).
do_port_out(_Addr, _Data) -> erlang:nif_error(undefined).

do_port_in32(_Addr) -> erlang:nif_error(undefined).
do_port_out32(_Addr, _Data) -> erlang:nif_error(undefined).

do_interrupt() -> erlang:nif_error(undefined).
do_get_sys_val(_Type) -> erlang:nif_error(undefined).
do_instruction(_Name) -> erlang:nif_error(undefined).
do_format(_) -> erlang:nif_error(undefined).
do_print_string(_) -> erlang:nif_error(undefined).

%%% CAPABILITY VALIDATORS

%% The general port verification interface.
validate_ports(Port) ->
	os_capabilities:validate_fail(
		ports,
		fun ports_details_match/2,
		Port
	).

%% Implements the semantics of the checker.
ports_details_match(Port, Cap) ->
	case Cap#capability.details of
		all -> true;
		{range, Start, End} when Port >= Start, Port =< End ->
			true;
		Port -> true;
		_ -> false
	end.

%% The general memory verification interface.
validate_memory(Addr, Length) ->
	os_capabilities:validate_fail(
		memory,
		fun memory_details_match/2,
		[Addr, Length]
	).

%% As with ports, but check ranges, too.
memory_details_match([Addr, Length], Cap) ->
	case Cap#capability.details of
		all -> true;
		{range, Start, End} when Addr >= Start, Addr + Length =< End ->
			true;
		Addr when Length == 1 -> true;
		_ -> false
	end.

%% Validate the generic system capability. This is just a presence check
%% in the capability list.
validate_system() ->
	os_capabilities:validate_fail(
		system,
		fun([], _Cap) -> true end,
		[]
	).
