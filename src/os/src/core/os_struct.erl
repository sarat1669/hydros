-module(os_struct).
-export([parse/2, write/2, size/1]).

%%% An interface for working with raw tables in memory, for example the SMBIOS
%%% and MP/MAD tables.

%% Take an address to read from, and a list describing the structure.
parse(_, []) -> [];
parse(Addr, Rec) when is_tuple(Rec) ->
	[RecName|Spec] = tuple_to_list(Rec),
	list_to_tuple(
		[
			RecName
		|
			[ Y || {_, Y} <- parse(Addr, [ {undefined, X} || X <- Spec ]) ]
		]
	);
parse(Addr, [H|Struct]) ->
	case H of
		Gap when is_integer(Gap) -> parse(Addr + Gap, Struct);
		{Name, Type} ->
			case Type of
				{OutType, Length} ->
					Data = os_unsafe:read(Addr, Length),
					[
						{
							Name,
							case OutType of
								binary -> Data;
								list -> binary_to_list(Data)
							end
						}
					|
						parse(Addr + Length, Struct)
					];
				DataType ->
					Size = val_size(DataType),
					[
						{
							Name,
							binary:decode_unsigned(
								os_unsafe:read(Addr, Size),
								little
							)
						}
					|
						parse(Addr + Size, Struct)
					]
			end
	end.

size(Rec) when is_tuple(Rec) ->
	?MODULE:size(tl(tuple_to_list(Rec)));
size(Struct) ->
	lists:sum(
		lists:map(
			fun({_, {Type, Len}}) -> val_size(Type) * Len;
				({_, Type}) -> val_size(Type);
				(AmVal) when is_atom(AmVal) -> val_size(AmVal);
				(Sz) -> Sz
			end,
			Struct
		)
	).

val_size(byte) -> 1;
val_size(char) -> 1;
val_size(short) -> 2;
val_size(int) -> 4;
val_size(long) -> 8.

write(_Addr, _Struct) ->
	error(not_implemented).
