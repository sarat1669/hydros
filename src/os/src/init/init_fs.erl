-module(init_fs).
-export([start/0]).

start() -> fs_server:start().

