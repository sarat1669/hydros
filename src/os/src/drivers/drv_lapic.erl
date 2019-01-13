-module(drv_lapic).
-export([start/0, start_aps/0, error_code/0, eoi/0]).
-export([init_timer/1, current_timer/0]). 

%%% Provides an interface through which various functions of
%%% the Local APIC can be accessed. Of particular note, this
%%% module provides a mechanism for starting other processors.

% The location of the LAPIC version register after relocation.
-define(LAPIC_BASE, 16#fee00000).

% A record that describes an IPI request
%-record(ipi, {
%	vector,
%	delivery_mode,
%	dest_mode,
%	level,
%	trigger_mode = 0,
%	dest_shorthand,
%	dest
%}).

start() -> enable_apic().

init_timer(TimerCount) ->
	% Start the timer
	write(timer_divide, 16#1),
	write(timer_count, TimerCount),
	write(lvt_timer, 16#20025).

enable_apic() -> write(spurious, 16#100).

eoi() ->
	%write(eoi, 0).
	os_unsafe:instruction(eoi).

current_timer() -> binary:decode_unsigned(read(timer_current)).

error_code() ->
	binary:decode_unsigned(read(error)).

%% IPI FUNCTIONS

%% Start all of the application processors.
start_aps() ->
	os_debug:log("Starting all APs..."),
	write(ipi, 16#CCD00),
	os_debug:log("Sent INIT IPI."),
	os_util:wait(100000),
	write(ipi, 16#CCE90),
%	os_util:wait(100000),
%	write(ipi, 16#CCE90),
	os_debug:log("Sent STARTUP IPIs."),
	ok.

%% Send an IPI
%ipi_send(I) -> ipi_write(ipi_build(I)).

%% Creates a binary that can be written onto the IPI memory register.
%% The definitions of these parameters can be found in section 10.6
%% of the Intel arch manuals, volume 3A.
%ipi_build(I) ->
	% The groupings in this binary are split by reserved bits
	%<<
	%	(I#ipi.vector):8, (I#ipi.delivery_mode):3, (I#ipi.dest_mode):1,
	%	0:2,
	%	(I#ipi.level):1, (I#ipi.trigger_mode):1,
	%	0:2,
	%	(I#ipi.dest_shorthand):2,
	%	0:12
	%>>.

%% HELPER FUNCTIONS

write(Reg, Data) when is_integer(Data) -> write(Reg, binary:encode_unsigned(Data));
write(Reg, Data) -> os_unsafe:write(?LAPIC_BASE + reg(Reg), bin_rev(Data), 32).

read(Reg) -> bin_rev(os_unsafe:read(?LAPIC_BASE + reg(Reg), 4)).

% Converts all of the registers from atoms to base offsets.
reg(id) -> 16#20;
reg(version) -> 16#30;
reg(task_priority) -> 16#80;
reg(processor_priority) -> 16#A0;
reg(eoi) -> 16#B0;
reg(spurious) -> 16#F0;
reg(error) -> 16#280;
reg(ipi) -> 16#300;
reg(lvt_timer) -> 16#320;
reg(lvt_lint0) -> 16#350;
reg(lvt_lint1) -> 16#360;
reg(timer_count) -> 16#380;
reg(timer_current) -> 16#390;
reg(timer_divide) -> 16#3E0.


% This really should not be necessary!
bin_rev(X) -> bin_rev(X, <<>>).
bin_rev(<<>>, Acc) -> Acc;
bin_rev(<<H:1/binary, Rest/binary>>, Acc) ->
    bin_rev(Rest, <<H/binary, Acc/binary>>).
