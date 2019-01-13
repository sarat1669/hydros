-module(drv_serial).
-export([init/1]).
-export([read/1, write/2]).

%%% A simple serial port driver.
%%% Mostly based on examples from the osdev wiki.

%% Initialise a serial port device.
init(DevID) ->
	% Disable interrupts temporarily
	drv_port:out(port(DevID, interrupt_enable), 16#0),
	% Enable DLAB
	drv_port:out(port(DevID, line_control), 16#80),
	% Set divisor to 3 (38400 baud)
	drv_port:out(port(DevID, divisor_low), 16#3),
	drv_port:out(port(DevID, divisor_high), 16#0),
	% 8 bit, no parity, one stop bit
	drv_port:out(port(DevID, line_control), 16#3),
	% Enable FIFO, clear, with 14-byte threshold
	drv_port:out(port(DevID, line_control), 16#80),
	% Interrupts enabled, RST/DSR set
	drv_port:out(port(DevID, line_control), 16#80).

%%% Data read/write functions
%%% Send or receive one byte from an init'd serial device.
%%% Functions block if data cannot be immediately sent or
%%% received.

%% Read data from a serial device.
read(DevID) ->
	os_util:until(fun() -> data_ready(DevID) end),
	drv_port:in(port(DevID, data)).

%% Check whether data is available to read.
data_ready(DevID) ->
	(drv_port:in(port(DevID, line_status)) band 1) == 1.

%% Write data to the device.
write(DevID, Data) ->
	os_util:until(fun() -> is_transit_empty(DevID) end),
	drv_port:out(port(DevID, data), Data).

%% Check whether we are ready to send a byte.
is_transit_empty(DevID) ->
	(drv_port:in(port(DevID, line_status)) band 16#20) =/= 0.

%%% Addressing functions.
%%% Converts atoms and device numbers to IO port addresses.
%%% port/2 should be the only entry point.

%% Calculate the IO port address for a register for a
%% given serial device.
port(Dev, Reg) ->
	dev_addr(Dev) + dev_reg(Reg).

%% Return a base IO port address for a given serial device.
dev_addr(1) -> 16#3F8;
dev_addr(2) -> 16#2F8;
dev_addr(3) -> 16#3E8;
dev_addr(4) -> 16#2E8.

%% Return a port offset for a given register.
% With DLAB set to low
dev_reg(data) -> 0;
dev_reg(interrupt_enable) -> 1;
% With DLAB set to high.
dev_reg(divisor_low) -> 0;
dev_reg(divisor_high) -> 1;
% With any DLAB
dev_reg(interrupt_id) -> 2;
dev_reg(line_control) -> 3;
dev_reg(modem_control) -> 4;
dev_reg(line_status) -> 5;
dev_reg(modern_status) -> 6;
dev_reg(scratch) -> 7.
