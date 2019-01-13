-module(drv_cursor).
-export([move_to/2]).

%%% A module that handles moving the cursor around the screen through the
%%% VGA hardware.

move_to(Col, Row) ->
	Pos = (Row * 80) + Col,
	drv_port:out(16#3D4, 16#0F),
	drv_port:out(16#3D5, Pos band 16#FF),
	drv_port:out(16#3D4, 16#0E),
	drv_port:out(16#3D5, Pos bsr 8).
