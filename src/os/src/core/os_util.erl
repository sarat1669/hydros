-module(os_util).
-export([wait/1, wait_ms/1, until/1]).
-export([if_core/2, if_not_core/2]).
-export([optional/1]).
-export([pad_to/2, number/1, number/2]).

%%% Various utility functions.

%% Busy wait N reductions
wait(0) -> ok;
wait(N) -> wait(N-1).

%% Run a function until it returns true (potentially with a value).
%% Useful for waiting for I/O registers etc.
until(F) ->
	case F() of
		true -> ok;
		{true, Val} -> Val;
		_ -> until(F)
	end.

%% Sleep X ms
wait_ms(X) ->
	receive after X -> ok end.

%% Execute function if this is node X
if_core(N, Fun) ->
	case os_system_info:get_proc_id() of
		N -> Fun();
		_ -> do_nothing
	end.

%% Execute function if this is NOT node X
if_not_core(N, Fun) ->
	case os_system_info:get_proc_id() of
		N -> do_nothing;
		_ -> Fun()
	end.

%% Optionaly execute a function on some data, depending on whether the
%% data is not the atom false.
optional(Data) ->
	fun(Fun) ->
		case Data of
			false -> false;
			_ -> Fun(Data)
		end
	end.

%% Pad a binary to the given size (IN BITS), little endian
pad_to(Bin, BitSize) ->
	Size = BitSize div 8,
	case (Size - size(Bin) rem Size) rem Size of
		0 -> Bin;
		N -> <<Bin/binary, 0:(N*8)>>
	end.

%% Number every element of a list. Optional second argument is the
%% starting number. Defautls to 1.
number(L) -> number(1, L).
number(_, []) -> [];
number(N, [H|T]) -> [{N, H}|number(N+1, T)].
