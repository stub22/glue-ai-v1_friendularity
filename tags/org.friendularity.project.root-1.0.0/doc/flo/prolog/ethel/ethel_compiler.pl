:- module(ethel_compiler, [compile/2]).
/** <module> Upper level predicates for the Ethel compiler

*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).
:- use_module(flo).
:- use_module(ethelparse).

%%	compile(+InFile:file, OutFile:file) is det
%
%	@arg InFile is one of stream(Stream), a stream handle, a file-URL or an atom that denotes a filename
%	@arg OutFile is one of stream(Stream), a stream handle, a file-URL or an atom that denotes a filename

compile(InFile, OutFile) :-
	file_graph_name(InFile, Graph),
	prep_for_file(Graph),
	phrase_from_file(ethel_program, InFile),
	rdf_save_turtle(OutFile, [graph(Graph)]).

%%	file_graph_name(?FileName:atom, ?GraphName:atom) is det
%
%	@arg the name of the file
%	@arg the corresponding graph
%
file_graph_name(File, File) :-
	File \= 'protos.ttl',
	File \= 'blockflow.ttl'.
file_graph_name(File, _) :-
	format(user_error, 'ILLEGAL FILE: The names protos.ttl and blockflow.ttl are reserved, don\'t use ~w~n', [File]),
	fail.

%%	prep_for_file(+Graph:atom) is det
%
%	prepare the system for loading
%	essentially this does all the stateful stuff
%
prep_for_file(Graph) :-
	ensure_normal_setup,
	rdf_unload_graph(Graph),
	rdf_create_graph(Graph),
	set_current_graph_name(Graph).






