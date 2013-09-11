:- module(ethelparse, [ethel_program//0]).
/** <module> Language definition for Ethel

    Ethel is a simple non visual language that compiles to flo.

*/

:- use_module(library(dcg/basics)).
:- use_module(flo).

ethel_program -->
	settings_section,
	blanks,
	"connect",
	blanks,
	ethel_body(none).

ethel_body(_) --> basics:eos.
ethel_body(Default) -->
	blank,
	ethel_body(Default).
ethel_body(Default) -->
	comment,
	ethel_body(Default).
ethel_body(Default) -->
	connection(Default, NewDefault),
	ethel_body(NewDefault).
ethel_body(Default) -->
	error,
	ethel_body(Default).


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
	eol.
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

package_part([H, "." |T]) -->
	id(H),
	".",
	package_part(T).
package_part([]) --> [].

connection(Default, RHS) -->
	blanks,
	terminal_name(Default, LHS),
	blanks,
	"=>",
	blanks,
	terminal_name(Default, RHS),
	{
	    create_connection(LHS, RHS)  % might backtrack
	}.

terminal_name(_, terminal(Name, Parm)) -->
	id(Name),
	blanks,
	"!",
	prolog_var_name(Type),
	blanks,
	":",
	blanks,
	{
	    create_named_block(Name, Type)
	},
	id(Parm).
terminal_name(_, terminal(Name, Parm)) -->
	"_!",
	blanks,
	prolog_var_name(Type),
	blanks,
	":",
	blanks,
	id(Parm),
	{
	    create_anonymous_block(Type, Name)
	}.
terminal_name(_, terminal(Name, Parm)) -->
	id(Name),
	blanks,
	":",
	blanks,
	id(Parm).
terminal_name(terminal(Name, _), terminal(Name, Parm)) -->
	{
            open_terminal(Name, Parm)
        },
	":".
terminal_name(terminal(Name, _), terminal(Name, Parm)) -->
	":",
	blanks,
	id(Parm).

terminal_name(_, terminal(Name, Parm)) -->
	id(Name),
	{
	   open_terminal(Name, Parm)
        },
	blanks,
	":".

terminal_name(_, java_terminal(Name, Type)) -->
	"j",
	blank,
	blanks,
	id(Type),
	blank,
	blanks,
	id(Name).

eol -->
	[X],
	{
	    code_type(X, end_of_line)
	}.

error -->
	[_],
	parser_restart.

parser_restart -->
	eol.
parser_restart -->
	[_],
	parser_restart.

settings_section --> [].
