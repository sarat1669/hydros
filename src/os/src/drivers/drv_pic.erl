-module(drv_pic).
-export([disable/0, eoi/0]).

%%% This module provides the functionality of disabling the PIC. This must be
%%% done before the ioapic or the local apic are enabled.

disable() ->
	% Restart the PICs
	os_unsafe:port_out(16#20, 16#11),
	os_unsafe:port_out(16#A0, 16#11),
	% Move interrupts
	os_unsafe:port_out(16#21, 16#30),
	os_unsafe:port_out(16#A1, 16#38),
	% Setup cascasding
	os_unsafe:port_out(16#21, 16#04),
	os_unsafe:port_out(16#A1, 16#02),
	os_unsafe:port_out(16#21, 16#01),
	os_unsafe:port_out(16#A1, 16#01),
	% Mask all interrupts
	os_unsafe:port_out(16#a1, 16#ff),
	os_unsafe:port_out(16#21, 16#ff).

eoi() ->
	os_unsafe:port_out(16#20, 16#20).

