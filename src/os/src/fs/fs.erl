-module(fs).
-export([write/2, read/1, getfiletable/0, pwb/0, cb/1, ls/0, cp/2, tagged/1]).

pwb() ->
	file_server ! {msg, working_bucket, self()},
	receive
		{allok, Currentbuck} -> Currentbuck
	end.

cb(Enter) ->
	file_server ! {msg, cb, Enter, self()},
	receive
		{allok} -> done
	end.

ls() ->
	file_server ! {msg, ls, self()},
	receive
		{allok,Bucketfiles} -> Bucketfiles
	end.

write(Variable, Filename) ->
	file_server ! {msg, write, Variable, Filename, self()},
	receive
		{allok} ->
			io:format("File was written successfully to ~s.~n",[Filename])
	end.

read(Filename) ->
	file_server ! {msg, read, Filename, self()},
	receive
		{allok, Term} -> Term;
		{nofile} -> io:format("No file of the name ~s was found.",[Filename])
	end.

cp(Filename, Bucket) ->
	file_server ! {msg, copy, Filename, Bucket, self()},
	receive
		{allok} -> io:format("The file ~s has been copied to ~s.~n",[Filename,Bucket]);
		{fail, Reason} -> io:format("The file could not be copied. The reason given was: ~s~n",[Reason])
	end.

%% Returns the current filetable in use by the system. Only included at the moment for testing purposes.
getfiletable() ->
	file_server ! {msg, filetable, self()},
	receive
		{allok, Term} -> io:format("Filetable was loaded~n")
	end,
	Term.

tagged(Taglist) ->
	file_server ! {msg, tagged, Taglist, self()},
	receive
		{allok, Results} -> Results
	end.
