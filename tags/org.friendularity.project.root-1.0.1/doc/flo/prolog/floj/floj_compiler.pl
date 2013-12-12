:- module(floj_compiler, [compile/3]).
/** <module> Upper level predicates for the Floj compiler
 *
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

:- license(apache).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).
:- use_module(ethel(flo)).
:- use_module(floj).

:- rdf_meta compile(+, +, r).

%%	compile(+InFile:file, +OutFile:file, +Node:r) is det
%
%
%	@arg InFile turtle file with blocks and java binding
%	blocks and java definition
%       @arg OutFile
%	a stream handle, a file-URL or an atom that denotes a filename
%	@arg Node is the name of a java class descriptor node

compile(InFile, OutFile, Node) :-
	ensure_normal_setup,
	rdf_load(InFile, [format(turtle)]),
	setup_call_catcher_cleanup(
	   open(OutFile, write, OutStream, [alias(compiler_out)]),
	   (
	       with_output_to(OutStream, write_java_class(Node))
	   ),
	   CompletionStatus,
	   (
	       print_error_message(CompletionStatus),
	       close(OutStream),
	       consider_removing_file(CompletionStatus, OutFile))
	).
compile(_, OutFile, _) :-
	format(user_error, 'Unable to open ~w~n', [OutFile]).

%%	print_error_message(+Status) is det
%
%	print an appropriate error message. If we exited normally do
%	nothing and succeed.
%
print_error_message(exit) :- !.
print_error_message( ! ) :- !.
print_error_message( fail ) :-
	format(user_error, 'write_java_class failed silently~n', []).
print_error_message(error(Formal, Context)) :-
	format(user_error, 'Prolog throws exception ~w~n', [error(Formal, Context)]).
print_error_message(X) :-
	format(user_effor, 'Floj error: ~w~n', [X]).

:- dynamic always_retain_files/0.

%%	consider_removing_file(+Status, +FileName) is det
%
%	examine the status. If we terminated with an error,
%	remove the file
%
consider_removing_file(_, _) :- always_retain_files, !.
consider_removing_file(exit, _) :- !.
consider_removing_file( ! , _) :- !.
consider_removing_file(_, File) :-
	access_file(File, write),
	delete_file(File).





