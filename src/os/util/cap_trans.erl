-module(cap_trans).
-export([parse_transform/2]).

%%% Add capability validators to exported functions.

%% A data structure that holds the capability settings found in a module.
-record(opts, {
	module,
	function,
	default,
	rejection,
	exports
}).

%% The main entypoint of the parse transformer.
%% Takes an AST, and returns a trnasformed AST.
parse_transform(AST, _) ->
	Os = get_opts(AST),
	NewAST = lists:map(fun(Expr) -> transform_fun(Expr, Os) end, AST),
	%io:format("Res: ~p~n", [NewAST]),
	NewAST.

%% Extract the parse transform options from the AST.
get_opts(AST) ->
	#opts {
		module = get_attr(module, AST),
		function = get_attr(capability, AST),
		default = get_attr(cap_default, AST),
		rejection = get_attr(cap_rejection, AST),
		exports = get_attr(export, AST)
	}.

%% Extract an attribute from the AST.
get_attr(_Name, []) -> undefined;
get_attr(Name, [{attribute, _Line, Name, Val}|_]) -> Val;
get_attr(Name, [_|R]) -> get_attr(Name, R).

%% Transform functions according to the settings derived from the module.
%% Must return the first argument if it does not match with a function
%% definition.
transform_fun(F = {function, Line, Name, Arity, Clauses}, Os) ->
	case lists:keyfind(Name, 1, Os#opts.exports) of
		{Name, Arity} ->
			% This is an exported function. Apply the transform.
			%io:format("TRANSFORMING FUNC: ~p.~n", [Clauses]),
			{
				function,
				Line,
				Name,
				Arity,
				lists:map(fun(C) -> transform_clause(C, Os) end, Clauses)
			};
		false ->
			% Function is not exported. Ignore it.
			F
	end;
transform_fun(Form, _Opts) -> Form.

%% Add a verification call to each clause.
transform_clause({clause, Line, Vars, Guards, Statements}, Os) ->
	{clause, Line, Vars, Guards,
		[
			{call, Line, {atom, Line, Os#opts.function}, []}
		|
			Statements
		]
	}.
