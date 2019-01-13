-module(drv_ioapic).
-export([id/0, arb_id/0, version/0, start/0]).
-export([enable_interrupt/1, disable_interrupt/1]).

%%% Provides an interface through which various functions of
%%% the IO APIC can be accessed.

% The location of the IOAPIC registers
-define(SELECT, 16#FEC00000).	% The register select register
-define(WIN,    16#FEC00010).	% The 'data' register
-define(REDIR_TAB, 16#10).		% The base of the redirection tables
-define(IRQ0, 16#20).			% The first IRQ
-define(INT_CPU, 0).			% CPU to which int's should be sent

%%% Public API

% Enable all of the IRQs
start() -> enable_all_interrupts().

% Get the IOAPIC ID
id() -> read(16#0) bsr 24.

% Get the IOAPIC ID
arb_id() -> read(16#2).

% Read the version of the IOAPIC.
version() -> read(16#1).

%%% Helper functions

enable_all_interrupts() ->
	lists:foreach(fun enable_interrupt/1, lists:seq(0, 15) -- exclusions()).

enable_interrupt(IRQ) ->
	os_debug:log("Enabling interrupt ~p.", [IRQ]),
	write(?REDIR_TAB + (2 * IRQ), ?IRQ0 + IRQ),
	write(?REDIR_TAB + (2 * IRQ) + 1, ?INT_CPU bsl 24).

% IRQs that we don't want to receive interupts from.
% Just the PIT, when the IOAPIC is initialized.
exclusions() -> [2].

disable_interrupt(IRQ) ->
	os_debug:log("Disabling interrupt ~p", [IRQ]),
	write(?REDIR_TAB + (2 * IRQ), 1 bsl 16),
	ok.

% Write to the IOAPIC
write(Reg, Data) ->
	%os_debug:log("Writing 0x~8.16.0B to 0x~8.16.0B.", [Data, Reg]),
	% Write the value repeatedly, until it is read back successfully.
	% Ensures that the value has been written to the selection register
	% before the value is written to the window register. This seems to
	% be a problem for Bochs sometimes.
%	os_util:until(
%		fun() ->
%			reg_write(select, Reg),
%			reg_read(select) == Reg
%		end
%	),
	reg_write(select, Reg),
	reg_write(win, Data).

% read from the IOAPIC
read(Reg) ->
	reg_write(select, Reg),
	reg_read(win).

%% Write to the register
reg_write(Addr, Int) ->
	os_unsafe:write(
		address(Addr),
		os_util:pad_to(binary:encode_unsigned(Int, little), 32),
		32
	).

%% Read from the register (returns a 32bit uint)
reg_read(Addr) ->
	binary:decode_unsigned(os_unsafe:read(address(Addr), 4), little).

address(select) -> ?SELECT;
address(win) -> ?WIN.
