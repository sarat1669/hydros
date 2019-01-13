-module(drv_terminal).
-export([start/0]).
-export([print/3, print/4, print/5, print/6]).
-export([get_line/2, put_line/3]).
-export([print_str/4, print_str/5, print_str/6]).
-export([rect/5, rect/6, rect/7]).
-export([print_boxed/6, print_boxed/7, print_boxed/8]).
-export([scroll/5, scroll_back/5]).
-export([hscroll/4, hscroll/5]).
-export([blit/1, blit/2, read/0, read/1]).

%%% This module defines a set of functions that can be used to write to and
%%% manipulate text output, from Erlang. These functions work on memory
%%% buffers, as well as the video output buffer.

%% Constants
-define(VID_BUF, 16#b8000).
-define(VID_HEIGHT, 25).
-define(VID_WIDTH, 80).

%% Defaults
-define(DEFAULT_CHAR, $  ).
-define(DEFAULT_FG, white).
-define(DEFAULT_BG, black).

%% Macros
% Take a description of what a character should look like, and produces the
% bytes for it.
-define(CHAR_BYTES(Char, BG, FG), << Char:8, (colour(BG)):4, (colour(FG)):4 >>).

%% This driver is stateless.
start() ->
	ok.

%%% PUBLIC INTERFACE FUNCTIONS

%% Print a given character at a location, with an optional foreground and
%% background colour. Returns the next char pos.
print(B, X, Y) -> print(B, X, Y, ?DEFAULT_CHAR).
print(B, X, Y, C) -> print(B, X, Y, C, ?DEFAULT_FG).
print(B, X, Y, C, FG) -> print(B, X, Y, C, FG, ?DEFAULT_BG).
print(B, X, Y, Char, FG, BG) ->
	write(pos_to_addr(B, X, Y), ?CHAR_BYTES(Char, BG, FG)).

%% Format and print the string (with args, if necessary), at the given
%% location.
% TODO: Should this be called _str, or _string?
print_str(B, X, Y, Str) -> print_str(B, X, Y, Str, ?DEFAULT_FG).
print_str(B, X, Y, Str, FG) when is_atom(FG) ->
	print_str(B, X, Y, Str, FG, ?DEFAULT_BG).
print_str(B, X, Y, Str, FG, BG) ->
	write(
		pos_to_addr(B, X, Y),
		<< ?CHAR_BYTES(Char, BG, FG) || Char <- Str >>
	).

%% Optionally format, then print some text in a box.
print_boxed(B, X, Y, X2, Y2, Str) -> print_boxed(B, X, Y, X2, Y2, Str, ?DEFAULT_FG).
print_boxed(B, X, Y, X2, Y2, Str, FG) -> print_boxed(B, X, Y, X2, Y2, Str, FG, ?DEFAULT_BG).
print_boxed(B, X, Y, X2, Y2, Str, FG, BG) ->
	% Draw and fill the rectangle, first.
	rect(B, X, Y, X2, Y2, BG, filled),
	% Print each of the characters, wrapping at X2 and scrolling when we fill up
	lists:foldl(
		fun(Char, {XN, YN}) ->
			% Print the current char
			print(B, XN, YN, Char, FG, BG),
			if XN == X2 -> {X, YN + 1};
			true -> {XN + 1, YN}
			end
		end,
		{X, Y},
		Str
	).

%% Clear a line, from X to X2. Does not touch the attribute bytes.
clear_line(_B, X, _, X2) when X > X2 -> ok;
clear_line(B, X, Y, X2) ->
	write(
		pos_to_addr(B, X, Y),
		<< ($ ):8 >>
	),
	clear_line(B, X + 1, Y, X2).

%% Scroll the contents of the box up, losing the top line. This line is
%% then returned from the function.
scroll(B, X, Y, X2, Y2) ->
	Line = os_unsafe:read(pos_to_addr(B, X, Y), (X2 - X + 1) * 2),
	do_scroll(B, X, Y, X2, Y2),
	Line.

do_scroll(B, X, Y, X2, Y) ->
	clear_line(B, X, Y, X2);
do_scroll(B, X, Y, X2, Y2) ->
	write(
		pos_to_addr(B, X, Y),
		os_unsafe:read(
			pos_to_addr(B, X, NextY = Y + 1),
			(X2 - X + 1) * 2 % 2 bytes per character!
		)
	),
	do_scroll(B, X, NextY, X2, Y2).

%% Scroll the contents of the box up, losing the top line. This line is
%% then returned from the function.
scroll_back(B, X, Y, X2, Y2) ->
	Line = os_unsafe:read(pos_to_addr(B, X, Y2), (X2 - X + 1) * 2),
	do_scroll_back(B, X, Y, X2, Y2 - 1),
	Line.

do_scroll_back(B, X, Y, X2, Y) ->
	clear_line(B, X, Y, X2);
do_scroll_back(B, X, Y, X2, Y2) ->
	write(
		pos_to_addr(B, X, Y2),
		os_unsafe:read(
			pos_to_addr(B, X, NextY2 = Y2 - 1),
			(X2 - X + 1) * 2 % 2 bytes per character!
		)
	),
	do_scroll(B, X, Y, X2, NextY2).

%% Scroll a horizontal, single line segment left or right.
hscroll(B, X, Y, Length) -> hscroll(B, X, Y, Length, left).
hscroll(B, X, Y, Len, Dir) ->
	write(
		pos_to_addr(B, X, Y),
		os_unsafe:read(
			pos_to_addr(
				B,
				X + case Dir of left -> 1; right -> -1 end,
				Y
			),
			Len * 2
		)
	),
	print(B, X + case Dir of left -> Len; right -> 0 end, Y, $  ).

%% Retreive a binary for an entire line of the output.
get_line(B, Num) ->
	os_unsafe:read(
		pos_to_addr(B, 0, Num),
		?VID_WIDTH * 2
	).

%% Place a binary in position at a line. Helpful for implementing
%% terminal scrolling.
put_line(B, Num, Bin) ->
	write(
		pos_to_addr(B, 0, Num),
		Bin
	).

rect(B, X, Y, X2, Y2) -> rect(B, X, Y, X2, Y2, ?DEFAULT_FG).
rect(B, X, Y, X2, Y2, Col) -> rect(B, X, Y, X2, Y2, Col, filled).
rect(_B, _X, Y, _X2, Y, _Col, _) -> ok;
rect(B, X, Y, X2, Y2, Col, _) ->
	print_str(B, X, Y, [ $  || _ <- lists:seq(1, X2 - X) ], white, Col),
	rect(B, X, Y + 1, X2, Y2, Col, filled).

%% Draw a rectangle, filled or unfilled.
%rect(B, X, Y, X2, Y2) -> rect(B, X, Y, X2, Y2, ?DEFAULT_FG).
%rect(B, X, Y, X2, Y2, Col) -> rect(B, X, Y, X2, Y2, Col, filled).
%rect(B, X, Y, X2, Y2, Col, Filled) ->
	% Generate a co-ord for every cell that needs to be filled.
	% If the last argument is not filled, the body-cells of the
	% rect will be filtered.
%	write(
%		pos_to_addr(B, X, Y),
%		<<
%			<< $ :8, (colour(Col)):4, (colour(white)):4  >>
%		||
%			YN <- lists:seq(X, X2),
%			XN <- lists:seq(Y, Y2),
%			% Shortcircuit this expr if we are painting everything,
%			% else, XN or YN must be equal to X or X2, or Y or Y2.
%			(Filled == filled) orelse
%			((XN == X) or (XN == X2) or (YN == Y) or (YN == Y2))
%		>>
%	).

%% Copy the contents of one buffer to another. Defaults to targeting
%% the system output. Can take either a binary or address for the source
blit(B) -> blit(?VID_BUF, B).
blit(Target, Addr) when is_integer(Addr) ->
	blit(
		Target,
		os_unsafe:read(
			pos_to_addr(Addr, 0, 0),
			?VID_WIDTH * ?VID_HEIGHT * 2
		)
	);
blit(DestAddr, Bin) when is_binary(Bin) ->
	write(
		pos_to_addr(DestAddr, 0, 0),
		Bin
	).

%% Read the current contents of the given buffer. Defaults to the text
%% output buffer.
read() -> read(?VID_BUF).
read(Addr) ->
	os_unsafe:read(
		pos_to_addr(Addr, 0, 0),
		?VID_WIDTH * ?VID_HEIGHT * 2
	).

write(Addr, Bin) when byte_size(Bin) > 8000 ->
	os_debug:log(warning, "Trying to write binary of size: ~p onto buffer ~p.",
		[byte_size(Bin), Addr]),
	write(Addr, binary:part(Bin, 0, 8000));
write(Addr, Bin) ->
	os_unsafe:write(Addr, Bin).

%%% HELPER FUNCTIONS

%% Converts and X and Y co-ordinate pair into a memory address.
pos_to_addr(B, X, Y) ->
	B
		+ (X * 2)
		+ (Y * 2 * ?VID_WIDTH).

%% Turns a colour atom into a nibble that is used to generate the
%% attribute byte. If given an integer which is in the correct
%% range to be a colour, the value is just passed through.
colour(black) ->		16#0;
colour(blue) ->			16#1;
colour(green) ->		16#2;
colour(cyan) ->			16#3;
colour(red) ->			16#4;
colour(magenta) ->		16#5;
colour(brown) ->		16#6;
colour(light_grey) ->	16#7;
colour(dark_grey) ->	16#8;
colour(light_blue) ->	16#9;
colour(light_green) ->	16#A;
colour(light_cyan) ->	16#B;
colour(light_red) ->	16#C;
colour(light_magenta) ->16#D;
colour(yellow) ->		16#E;
colour(white) ->		16#F;
colour(X) when is_integer(X) and X =< 16 -> X.
