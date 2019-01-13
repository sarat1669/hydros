-module(drv_port).
-export([in/1, out/2, in_bin/1, out_bin/2]).

%%% Functions that interact with the system's ports.

in(Addr) -> os_unsafe:port_in(Addr).

out(Addr, Data) -> os_unsafe:port_out(Addr, Data).

in_bin(Addr) -> os_unsafe:port_in_bin(Addr).

out_bin(Addr, Data) -> os_unsafe:port_out_bin(Addr, Data).
