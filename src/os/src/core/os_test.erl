-module(os_test).
-export([hash/2]).
-export([port_ok/0, port_fail/0]).

-include("cap.hrl").

%%% A module that contains temporary OS system testing functions.
%%% A scratch pad, if you will.

%% Use the hasher unikernel to hash a string a number of times.
hash(Str, Reps) ->
	os_ipc:send(
		1,
		self(),
		<<
			Reps:64/integer-little,
			(list_to_bitstring(Str))/binary,
			0
		>>
	),
	receive
		Bin -> binary:decode_unsigned(Bin)
	end.

port_ok() ->
	os_capabilities:spawn(
		fun() ->
			io:format("Port res: ~p~n", [os_unsafe:port_in(16#64)])
		end,
		[
		]
	).

port_fail() ->
	os_capabilities:spawn(
		fun() ->
			io:format("Port res: ~p~n", [os_unsafe:port_in(16#65)])
		end,
		[
			#capability {
				name = ports,
				type = allow,
				details = 16#64
			},
			#capability {
				name = ports,
				type = disallow,
				details = all
			}
		]
	).
