-module(load_os_package).
-export([start/0]).

start() ->
	erlang:display(loading_os),
	foreach(fun load_code/1, modules()),
	erlang:display(loaded_os),
	ok.

load_code({Module, Binary}) ->
	erlang:display(Module),
	erlang:load_module(Module, Binary),
	ok.

foreach(_, []) -> ok;
foreach(Fun, [H|T]) ->
	Fun(H),
	foreach(Fun, T).

modules() ->
	binary_to_term(os_unsafe:get_os_package()).
