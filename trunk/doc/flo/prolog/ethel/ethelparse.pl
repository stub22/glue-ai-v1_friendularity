:- module(ethelparse, []).

:- use_module(library(dcg/basics)).

ethel_program(_, _) --> basics:eos.
ethel_program(Settings, State) -->
	blank,
	ethel_program(Settings, State).
ethel_program(Settings, State) -->
	comment,
	ethel_program(Settings, State).
ethel_program(Settings, State) -->
	setting_stmt(Settings, NewSettings),
	ethel_program(NewSettings, State).
ethel_program(Settings, State) -->
	connection(Settings, State, NewState),
	ethel_program(Settings, NewState).
ethel_program(Settings, State) -->
	error,
	ethel_program(Settings, State).


comment -->
	"/*",
	comment_body.
comment -->
	"//",
	line_comment.


comment_body -->
	"*/", !.
comment_body -->
	[_],
	comment_body.

line_comment -->
	[X],
	{
	    code_type(X, end_of_line)
	}.
line_comment -->
	[X],
	{
	    \+ code_type(X, end_of_line)
	},
	line_comment.



setting_stmt(Settings, [generate_java_as(Pkg, Name) | NewSettings]) -->
	"javaclass",
	fully_qualified_java_name(Pkg, Name),
	{
	    subtract(Settings, [generate_java_as(_,_)], NewSettings)
	}.

fully_qualified_java_name(Pkg, Name) -->
	package_part(PkgParts),
	{
	    flatten(PkgParts, Pkg)
	},
	id(Name).


id([H|T]) -->
	[H],
	{
	    code_type(H, csymf)
	},
	id_rest(T).

id_rest([H|T]) -->
	[H],
	{
	    code_type(H, csym)
	},
	id_rest(T).

id_rest([]) --> [].

package_part([H|T]) -->
	id(H),
	".",
	package_part(T).
package_part([]) --> [].

% TODO semantics
connection(Settings, State, NNState) -->
	blanks,
	terminal_name(State, NState, LHS),
	blanks,
	"=>",
	blanks,
	terminal_name(NState, NNState, RHS).

/*
	DONE TO HERE
terminal_name(State, NState, terminal(Name, Parm)) -->
	id(Name),
	":",
*/






