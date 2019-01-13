-module(drv_cmos).
-export([local_time/0, unix_time/0]).

% Define the locations of the control and data ports.
-define(CONTROL, 16#70).
-define(DATA, 16#71).

%% Get the local time from the CMOS ports
local_time() ->
	% Run the function until we get the same value twice,
	% in order to avoid strange values from updates.
	os_util:until(
		fun() ->
			XT = raw_local_time(),
			case raw_local_time() == XT of
				true -> {true, XT};
				X -> X
			end
		end
	).

unix_time() ->
	{{Yr, Mo, Da}, {Hr, Mi, Se}} = local_time(),
	((Yr - 1970) * (60 * 60 * 24 * 365)) +
		(Mo * (60 * 60 * 24 * 28)) +
		(Da * (60 * 60 * 24)) +
		(Hr * (60 * 60)) +
		(Mi * 60) +
		Se.

raw_local_time() ->
	os_util:until(fun() -> not updating() end),
	{
		{
			(bcd(read(century)) * 100) + bcd(read(year)),
			bcd(read(month)),
			bcd(read(day))
		},
		{
			handle_24_hour(bcd_hour(read(hour))),
			bcd(read(minute)),
			bcd(read(second))
		}
	}.

%% If the CMOS is in 24h mode, convert.
handle_24_hour(X) ->
	case is_24_hour() and ((X band 16#80) =/= 0) of
		false -> X;
		true -> ((X band 16#7F) + 12) rem 24
	end.

%% If the CMOS is using BCD, convert it to normal binary encoding.
bcd(X) ->
	case is_bcd() of
		false -> X;
		true -> (X band 16#0F) + ((X div 16) * 10)
	end.

%% Un-BCD for the hour register
bcd_hour(X) ->
	case is_bcd() of
		false -> X;
		true -> ((X band 16#F) + (((X band 16#70) div 16) * 10)
					bor (X band 16#80))
	end.

%% Detect whether the CMOS is using BCD mode
is_bcd() -> (read(status_b) band 16#4) == 0.

%% Detect whether the CMOS is using BCD mode
is_24_hour() -> (read(status_b) band 16#2) == 1.
	
%% Are the CMOS time registers being updated?
updating() -> (read(status_a) band 16#80) =/= 0.

read(Reg) ->
	drv_port:out(?CONTROL, reg(Reg)),
	drv_port:in(?DATA).

%% Translate register names into control port values.
reg(second) ->		16#0;
reg(minute) ->		16#2;
reg(hour) ->		16#4;
reg(weekday) ->		16#6;
reg(day) ->			16#7;
reg(month) ->		16#8;
reg(year) ->		16#9;
reg(century) ->		16#32;
reg(status_a) ->	16#A;
reg(status_b) ->	16#B.
