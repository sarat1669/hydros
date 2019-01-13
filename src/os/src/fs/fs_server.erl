-module(fs_server).
-export([start/0, server/3]).

%% Gets the size of the filetable from sector 19000, as well as the start write sector.
%% If the sector is a null binary, creates a new filetable & binary. Otherwise, reads the filetable from appropriate sectors and
%% starts the fileserver.

start() ->
	Fileserverbinary = drv_isa:read(0),
	case isnull(Fileserverbinary) of
		true -> register(file_server, spawn(?MODULE, server, [[],300,"/"]));
		false -> [Filetablesize, Startingsector] = unconv(Fileserverbinary),
			Numberoffiletablesectors = (Filetablesize div 512),
			Filetable = get_filetable(1, Numberoffiletablesectors),
			register(file_server, spawn(?MODULE, server, [Filetable, Startingsector,"/"]))
	end.

%% Fileserver exists to listen to requests from the fs module in order to retrieve or store variables from/into filenames.
%% If the fs module requests, will also return the filetable to the user.
%% TODO: Implement advanced functionality for further file manipulation. Implement way of overwriting files if an in-use filename is passed with another variable to write.
server(Filetable, Num, Bucket) ->
        receive
                {msg, write, Variable, Filename, Caller} ->
			{Appendedbinaries, Sectorstowriteto, Firstsector, Lastsector} = start(Variable, Num),
			write_multiple_sectors(Sectorstowriteto, Appendedbinaries),
			Size = byte_size(term_to_binary(Variable)),
			Newfilename = fixfilename(Bucket, Filename, Filetable, std),
			Newfiletable = Filetable ++ [{Newfilename, Firstsector, Size}],
			Newftbinary = convfull(Newfiletable),
			write_file_table(Newftbinary),
			Startingsector = Lastsector+1,
			Newftsize = byte_size(Newftbinary),
			Fileserverbinary = convfull([Newftsize,Startingsector]),
			drv_isa:write(0, Fileserverbinary),
                        Caller ! {allok},
                        server(Newfiletable, Startingsector,Bucket);
                {msg, read, Filename, Caller} ->
			Filetoread = filetoread(Filename, Bucket),
			Member = lists:keymember(Filetoread,1,Filetable),
			case Member of
				true ->	{_,Firstsector,_} = lists:keyfind(Filetoread,1,Filetable),
		                        Term = readstart(Firstsector),
					Caller ! {allok, Term};
				false -> Caller ! {nofile}
			end,
                        server(Filetable, Num,Bucket);
		{msg, copy, Filename, Path, Caller} ->
			case isspecial(Path) of
				true ->
					Caller ! {fail, "Cannot copy to special path names yet.~n"},
					Newfiletable = Filetable;
				false -> 
					Fileid = getrelid(Filename),
					Buckid = fixbuckname(Path,Bucket),
					Realfilename = filetoread(Filename,Bucket),
					case lists:keymember(Realfilename,1,Filetable) of
						true -> {_,Firstsector,Size} = lists:keyfind(Realfilename,1,Filetable),
							Newfilename = fixfilename(Buckid,Fileid,Filetable,cp),
							Newfiletable = Filetable ++ [{Newfilename, Firstsector, Size}],
							Newftbinary = convfull(Newfiletable),
							Newftsize = byte_size(Newftbinary),
							write_file_table(Newftbinary),
							[_,Startingsector] = unconv(drv_isa:read(0)),
							Fileserverbinary = convfull([Newftsize, Startingsector]),
							drv_isa:write(0,Fileserverbinary),
							Caller ! {allok};
						false -> Caller ! {fail, "This file does not exist."},
							Newfiletable = Filetable
					end
			end,
			server(Newfiletable, Num, Bucket);
		{msg, filetable, Caller} ->
			Caller ! {allok, Filetable},
			server(Filetable, Num,Bucket);
		{msg, fileserverterm, Caller} ->
			Fileserverbinary = drv_isa:read(7),
			Fileserverterm = unconv(alttrim(Fileserverbinary)),
			Caller ! {allok, Fileserverterm},
			server(Filetable, Num,Bucket);
		{msg, firstblanksector, Sectorafternum, Caller} ->
			Firstblanksector = firstblanksectorafter(Sectorafternum),
			Caller ! {allok, Firstblanksector},
			server(Filetable, Num,Bucket);
		{msg, nextsectors, Amount, Caller} ->
			Nextsectorlist = nextsector(Num+1, Amount),
			Caller ! {allok, Nextsectorlist},
			server(Filetable, Num,Bucket);			
		{msg, numberoffreesectors, Number, Caller} ->
			Amount = numberoffreesectors(Number),
			Caller ! {allok, Amount},
			server(Filetable,Num,Bucket);
		{msg, working_bucket, Caller} ->
			Caller ! {allok, Bucket},
			server(Filetable,Num,Bucket);
		{msg, cb, Enter, Caller} ->
			Firstletter = string:substr(Enter,1,1),
			Lastletter = string:substr(Enter,length(Enter),1),
			Dirname = calculate(Enter, Firstletter, Lastletter, Bucket),
			Caller ! {allok},
			server(Filetable,Num,Dirname);
		{msg, ls, Caller} ->
			Bucketfiles = getlist(Bucket,Filetable),
			Caller ! {allok, Bucketfiles},
			server(Filetable,Num,Bucket);
		{msg, tagged, Taglist, Caller} ->
			Results = gettagged(Filetable, Taglist),
			Caller ! {allok, Results},
			server(Filetable,Num,Bucket);
		{msg, kill} ->
			done
        end.
	

calculate(Enter, Firstletter, Lastletter, Bucket) ->
	case Enter == ".." of
	false -> 
		case Firstletter of
			"/" ->
				case Lastletter == "/" of
					true -> Enter;
					false -> Enter++"/"
				end;
			_ ->
				case Lastletter == "/" of
					true -> Bucket++Enter;
					false -> Bucket++Enter++"/"
				end
		end;
	true -> 
		case Enter == "/" of
			true -> 
				"/";
			false -> 
				string:sub_string(Bucket,1,string:rchr(string:sub_string(Bucket,1,length(Bucket)-1),$/))
		end
	end.

filetoread(Filename, Bucket) ->
	case string:substr(Filename,1,1) == "/" of
		true -> Filename;
		false -> Bucket++Filename
	end.

%% Returns the filetable.
get_filetable(Startingsector, Numberofsectors) ->
	Binarylist = return_binary(Startingsector, Numberofsectors),
	Binary = list_to_binary(Binarylist),
	Filetable = unconv(Binary),
	Filetable.
	
%% Returns the binary at sector "Startingsector".
return_binary(Sector, Numberofsectors) ->
	return_binary(Sector, Numberofsectors, 1).

return_binary(Sector, Numberofsectors, Whichnum) ->
	case Numberofsectors == Whichnum of
		true -> [drv_isa:read(Sector)];
		false -> [drv_isa:read(Sector)] ++ return_binary(Sector+1, Numberofsectors, Whichnum+1)
	end.

%% Writes the file table binary.
write_file_table(Filetablebinary) ->
	Numberofsectorstowrite = byte_size(Filetablebinary) div 512,
	write_file_table(Filetablebinary, Numberofsectorstowrite, 1).

write_file_table(Filetablebinary, 1, Thissector) ->
	drv_isa:write(Thissector, Filetablebinary);

write_file_table(Filetablebinary, Numberofsectorstowrite, Thissector) ->
	Thisbin = binary_part(Filetablebinary, 0, 512),
	Newbinlength = byte_size(Filetablebinary) - 512,
	Restofbin = binary_part(Filetablebinary, 512, Newbinlength),
	drv_isa:write(Thissector, Thisbin),
	write_file_table(Restofbin, Numberofsectorstowrite - 1, Thissector + 1).	

%% Finds the first blank sector to write to.
%% NB: Currently this causes HydrOS to page fault (unrecoverable) if too many consecutive used sectors. This is because
%% it will try reading too many sectors in too short a space of time, so will suffer an interrupt.
%% This can be fixed but quite complex, at the moment this method is still useful as it acts as a
%% null check to ensure no overwriting of already stored data.
firstblanksectorafter(Num) ->
	Bin = drv_isa:read(Num),
	case isnull(Bin) of
		true -> Num;
		false -> firstblanksectorafter(Num+1)
	end.

numberoffreesectors(Num) ->
	Sec = drv_isa:read(Num),
	case isnull(Sec) of
		true -> 1 + numberoffreesectors(Num+1);
		false -> 0
	end.

nextsector(Nextsector, 1) ->
	[firstblanksectorafter(Nextsector)];
	
nextsector(Nextsector, Howmany) ->
	Thissectorfound = firstblanksectorafter(Nextsector),
	[Thissectorfound] ++ nextsector(Thissectorfound+1, Howmany - 1).

%%% READING AND WRITING MULTI-SECTOR BINARIES	
	
write_multiple_sectors(Sectorstowriteto, Appendedbinaries) ->
	Numberofthingstowrite = length(Sectorstowriteto),
	write_multiple_sectors(Sectorstowriteto, Appendedbinaries, Numberofthingstowrite).

write_multiple_sectors(Sectorstowriteto, Appendedbinaries, 1) ->
	Sector = lists:nth(1, Sectorstowriteto),
	Binary = lists:nth(1, Appendedbinaries),
	drv_isa:write(Sector, Binary);

write_multiple_sectors(Sectorstowriteto, Appendedbinaries, Numberofthingstowrite) ->
	Sector = lists:nth(Numberofthingstowrite, Sectorstowriteto),
	Binary = lists:nth(Numberofthingstowrite, Appendedbinaries),
	drv_isa:write(Sector, Binary),
	write_multiple_sectors(Sectorstowriteto, Appendedbinaries, Numberofthingstowrite - 1).

calculatesectorstoappend(Sectorstowriteto) ->
        Toappend = lists:sublist(Sectorstowriteto, 2, length(Sectorstowriteto)-1),
        Finaltoappend = Toappend ++ [0],
        Finaltoappend.




%%%%%%%IMPORTED CONV%%%%%%%%

conv(Variable) ->
	Unpadded = term_to_binary(Variable),
	Padded = pad(Unpadded),
	Padded.
	
%% Takes a binary and pads it to a binary of 508 bytes or one of its multiples.

convfull(Variable) ->
	Binary = term_to_binary(Variable),
	Length = byte_size(Binary),
	Remainder = Length rem 512,
	Padnumber = 512 - Remainder,
	padit(Padnumber, Binary).

pad(Binary) ->
	Length = byte_size(Binary),
	Remainder = Length rem 508,
	Padnumber = 508 - Remainder,
	padit(Padnumber, Binary).


%% Final-state function for padit
padit(0, Binary) ->
	Binary;
	
%% Adds a 0 to the binary, then calls itself with the new binary with the value Topad reduced by 1
padit(Topad, Binary) ->
	Nextbinary = <<Binary/binary,0>>,
	padit(Topad - 1, Nextbinary).

%% Takes a binary and then converts it back to a term. At the moment it merely does binary_to_term but will
%% grow for extra functionality
unconv(Binary) ->
	Unconverted = binary_to_term(Binary),
	Unconverted.

%%%%%%%%%%%%%%%%%%%% BINARY + SECTOR CREATION %%%%%%%%%%%%%%%%%%%%%%

start(Variable, Num) ->
	Paddedbinary = conv(Variable),
	Numberofblobs = numblocks(Paddedbinary),
	Binarylist = break(Paddedbinary, 1, Numberofblobs),
	Sectorstowriteto = nextsector(Num, length(Binarylist)),
	Sectorstoappend = convertsectorlist(calculatesectorstoappend(Sectorstowriteto),encode),
	Appendedbinaries = appendsectors(Binarylist,Sectorstoappend),
	Firstsector = lists:nth(1, Sectorstowriteto),
	Lastsector = lists:nth(length(Sectorstowriteto), Sectorstowriteto),
	{Appendedbinaries, Sectorstowriteto, Firstsector, Lastsector}.

sectorpad(Binary) ->
        Size = byte_size(Binary),
        case Size >= 4 of
                true -> Binary;
                false -> sectorpad(Binary, Size)
        end.

sectorpad(Binary, Size) ->
        case Size == 4 of
                true -> Binary;
                false -> Newbinary = <<0,Binary/binary>>,
                        sectorpad(Newbinary, Size+1)
        end.

convertsectorlist([], encode) ->
                [];
convertsectorlist([H|T], encode) ->
                [sectorpad(binary:encode_unsigned(H))|convertsectorlist(T,encode)];
convertsectorlist([], decode) ->
                [];
convertsectorlist([H|T], decode) ->
                [binary:decode_unsigned(H)|convertsectorlist(T,decode)].


break(Binary, Numofblocks, Numofblocks) ->
                        [Binary];

break(Binary, Which, Numofblocks) ->
                        Part = binary_part(Binary, 0, 508),
                        Rest = binary_part(Binary, 508, byte_size(Binary)-508),
                        [Part] ++ break(Rest, Which+1, Numofblocks).

numblocks(Binary) ->
        Size = byte_size(Binary),
        case Size =< 508 of
                true -> 1;
                false ->
                        Rest = binary_part(Binary, 508, byte_size(Binary)-508),
                        1 + numblocks(Rest)
        end.

appendsectors([],[]) -> [];
appendsectors(Listofblobs, Convertedsectorlist) ->
        [Headofblobs|Tailofblobs] = Listofblobs,
        [Headofsecs|Tailofsecs] = Convertedsectorlist,
        [<<Headofblobs/binary,Headofsecs/binary>>|appendsectors(Tailofblobs,Tailofsecs)].


readstart(Sector) ->
	List = getbinarylist(Sector),
	Binary = list_to_binary(List),
	Variable = unconv(Binary),
	Variable.

getbinarylist(Sector) ->
	{Dataportion, Nextsector} = getsectorreference(Sector),
	case Nextsector == 0 of
		true -> [Dataportion];
		false -> [Dataportion] ++ getbinarylist(Nextsector)
	end. 

getsectorreference(Sector) ->
	Binary = drv_isa:read(Sector),
	Dataportion = binary_part(Binary, 0, 508),
	Sectorportion = binary_part(Binary, 508, 4),
	Nextsector = binary:decode_unsigned(Sectorportion),
	{Dataportion, Nextsector}.

fixfilename(Bucket,Filename, Filetable, std) ->
	Newfilename = Bucket++re:replace(Filename,"/","",[global,{return,list}]),
	exists(Newfilename,Filetable);
fixfilename(Bucket,Filename,Filetable,cp) ->
	Newfilename = Bucket++Filename,
	exists(Newfilename,Filetable).	

exists(Filename, Filetable) ->
	exists(Filename, Filetable, 1).

exists(Filename, Filetable, Num) ->
	Length = length(Filetable),
	case Num =< Length of
		true -> 
			{X,_,_} = lists:nth(Num, Filetable),
			case Filename == X of
				true -> 
					Newfilename = Filename ++ "~",
					exists(Newfilename, Filetable, Num);
				false ->
					exists(Filename, Filetable, Num+1)
			end;
		false ->
			Filename
	end.

getlist(Key,List) ->
		Result = isfound(Key,List),
		filter(Key,Result).

isfound(_,[]) ->
                [];
isfound(Key,List) ->
                [H|T] = List,
                {Hkey,_,_} = H,
                case begins(Key,Hkey) of
                        true -> [Hkey] ++ isfound(Key,T);
                        false -> isfound(Key,T)
                end.

begins(Key,Value) ->
                case string:str(Value,Key) of
                        1 -> true;
                        _ -> false
                end.

filter(_,[]) ->
		[];
filter(Key,List) ->
		[H|T] = List,
		Result = re:replace(H,Key,"",[{return,list}]),
		[Result] ++ filter(Key,T).

%% Checks if a binary is a 512-byte binary full of 0s.
isnull(A) ->
	case A == <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>> of
		true -> true;
		false -> false
	end.

%% Creates a 512-byte binary full of 0s. Future implementation in secure delete
%null() ->
%<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>.

%% Removes all trailing 0s from a binary. Deprecated - use alttrim instead
%trim(A) ->
%	case binary:at(A,byte_size(A) - 1) == 0 of
%		true -> trim(binary:part(A,{0,byte_size(A)-1}));
%		false -> A
%	end.

%% Alternative method to remove all trailing 0s from a binary
alttrim(A) ->
	list_to_binary(lists:reverse(lists:dropwhile(fun(0) -> true; (_) -> false end, lists:reverse(binary_to_list(A))))).

%% Currently unused.
%binary_join([], _Sep) ->
%  <<>>;
%
%% Converts a list containing a single binary into a standalone binary
%binary_join([Part], _Sep) ->
%  Part;
%
%% Takes a list of binaries and shoves them together, with the binary of value defined in Sep to separate them
%binary_join(List, Sep) ->
%  lists:foldr(fun (A, B) ->
%    if
%      bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
%      true -> A
%    end
%  end, <<>>, List).

getrelid(Filename) ->
	string:sub_string(Filename,string:rchr(string:sub_string(Filename,1,length(Filename)-1),$/)+1,length(Filename)).

fixbuckname(Path,Currentbuck) ->
		case string:substr(Path,1,1) == "/" of
			true -> 
				case string:substr(Path,length(Path),1) == "/" of
					true -> Path;
					false -> Path++"/"
				end;
			false -> 
				case string:substr(Path,length(Path),1) == "/" of
					true -> Currentbuck ++ Path;
					false -> Currentbuck ++ Path ++ "/"
				end
		end.	

gettagged(Filetable,List) ->
        Bucketedfiletable = getbuckets(Filetable),
        compare(Bucketedfiletable,List).

compare([],_List) ->
	[];
compare(Filetable,List) ->
        [H|T] = Filetable,
        {Filename,Buckets} = H,
        case match(Buckets,List) of
		true -> [Filename] ++ compare(T,List);
		false -> compare(T,List)
	end.

getbuckets([]) ->
	[];
getbuckets(Filetable) ->
        [H|T] = Filetable,
        {Filename,_,_} = H,
        Split = re:split(Filename,"/",[{return,list}]),
        Buckets = lists:sublist(Split,2,length(Split)-2),
        Record = {Filename,Buckets},
        [Record] ++ getbuckets(T).

match(_Filebuckets, []) ->
	true;
match(Filebuckets, Requestedbuckets) ->
        [Head|Tail] = Requestedbuckets,
        case lists:member(Head,Filebuckets) of 
		true -> match(Filebuckets,Tail);
		false -> false
	end.

isspecial(Path) ->
	case Path of
		".." -> true;
		"." -> true;
		"~" -> true;
		_ -> false
	end.
