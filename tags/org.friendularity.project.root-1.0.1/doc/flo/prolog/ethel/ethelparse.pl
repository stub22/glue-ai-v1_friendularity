:- module(ethelparse, [ethel_program//0]).
/** <module> Language definition for Ethel

    Ethel is a simple non visual language that compiles to flo.

==
// An Ethel program - comments can be C style, at stmt level
// or double slash end of line style

java com.example.ExampleBlock

connect

_!camera:O => _!gray:A => t!threshold:A
publish t:O as ghosty
float(0.5) => t:T

==

An Ethel program has two sections, a 'settings' section and a
'connections' section


*/

:- use_module(library(dcg/basics)).
:- use_module(flo).

%%	ethel_program// is det
%
%	main DCG for the Ethel parser
%
%	An Ethel program consists of settings
%	the keyword "connect"
%	and a series of connections
%
ethel_program -->
	settings_section,
	blanks,
	"connect",
	blanks,
	ethel_body(none).

%%	ethel_body(+Current_Block)// is det
%
%	Generate the rest of the body
%	The current block is which block will be
%	used for references
%
ethel_body(_) --> dcg_basics:eos.
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
	publish_cmd(Default),
	ethel_body(Default).
ethel_body(Default) -->
	subscribe_cmd(Default),
	ethel_body(Default).
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

settings_section -->
	java_class_stmt,
	settings_section.
settings_section --> [].

:- dynamic user:java_class/3.

java_class_stmt -->
	"java",
	blank,
	blanks,
	fully_qualified_java_name(Pkg, Name),
	blanks_to_nl,
	{
	    atom_codes(APkg, Pkg),
	    atom_codes(AName, Name),
	    create_java_class(APkg, AName, URI),
	    asserta(user:java_class(APkg, AName, URI))
	}.

fully_qualified_java_name(Pkg, Name) -->
	package_part(PkgParts),
	{
	    flatten(PkgParts, PkgDot),
	    append(Pkg, ".", PkgDot)
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
	    terminal_to_atom(LHS, ALHS),
	    terminal_to_atom(RHS, ARHS),
	    create_connection(ALHS, ARHS)  % might backtrack
	}.

publish_cmd(Default) -->
	blanks,
	"publish",
	blanks,
	terminal_name(Default, Terminal),
	blanks,
	"as",
	blanks,
	id(Name),
	blanks,
	{
	    create_publish_node(Name, Terminal)
	}.

subscribe_cmd(Default) -->
	blanks,
	"subscribe",
	blanks,
	id(Name),
	blanks,
	"to",
	terminal_name(Default, Terminal),
	blanks,
	{
	    create_subscribe_node(Name, Terminal)
	}.


terminal_to_atom(terminal(CName, CParm), terminal(AName, AParm)) :-
	atom_codes(AName, CName),
	atom_codes(AParm, CParm).

terminal_name(_, terminal(Name, Parm)) -->
	"_!",
	!,
	blanks,
	prolog_atom_name(Type),
	blanks,
	":",
	blanks,
	id(Parm),
	{
	    atom_codes(AType, Type),
	    create_anonymous_block(AType, AName),
	    atom_codes(AName, Name)
	}.
terminal_name(_, terminal(Name, Parm)) -->
	id(Name),
	blanks,
	"!",
	prolog_atom_name(Type),
	blanks,
	":",
	blanks,
	id(Parm),
	{
	    atom_codes(AName, Name),
	    atom_codes(AType, Type),
	    create_named_block(AName, AType)
	}.
terminal_name(_, terminal(Name, Parm)) -->
	id(Name),
	blanks,
	":",
	blanks,
	id(Parm).
terminal_name(terminal(Name, _), terminal(Name, Parm)) -->
	":",
	{
	    atom_codes(AName, Name),
            open_terminal(AName, AParm),
	    atom_codes(AParm, Parm)
        }.
terminal_name(terminal(Name, _), terminal(Name, Parm)) -->
	":",
	blanks,
	id(Parm).

terminal_name(_, terminal(Name, Parm)) -->
	id(Name),
	blanks,
	":",
	{
	   open_terminal(Name, Parm)
        }.

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

prolog_atom_name([H|T]) -->
	[H],
	{
	    code_type(H, prolog_atom_start)
	},
	prolog_atom_rest(T).

prolog_atom_rest([H|T]) -->
	[H],
	{
	    code_type(H, prolog_atom_start)
	},
	prolog_atom_rest(T).
prolog_atom_rest([]) --> [].

nl --> "\r\n".
nl --> "\n".
