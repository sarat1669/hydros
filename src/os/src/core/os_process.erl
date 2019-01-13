-module(os_process).
-export([get_node_id/1]).
-export([pid_to_binary/1, binary_to_pid/1]).

get_node_id(_PID) -> erlang:nif_error(undefined).

pid_to_binary(_PID) -> erlang:nif_error(undefined).

binary_to_pid(_PID) -> erlang:nif_error(undefined).
