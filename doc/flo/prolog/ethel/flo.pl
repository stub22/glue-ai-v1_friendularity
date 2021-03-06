:- module(flo, [ensure_normal_setup/0,
	        create_connection/2,
	       set_current_graph_name/1,
	       create_named_block/2,
	       create_anonymous_block/2,
	       open_terminal/2,
	       create_java_class/3,
	       create_publish_node/2,
	       create_subscribe_node/2]).
/** <module> Predicates for manipulating the Flo rdf language

*/

:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uuid)).

:- rdf_register_prefix(flo, 'http://www.friendularity.org/ontology/flo#').
:- dynamic flostate:setup_normally/1.

:- rdf_meta assert_rdf_default(r, r, o),
	input_terminal(+, +, r, r),
	output_terminal(+, +, r, r),
	not_connected_input(r, -),
	not_connected_output(r, -),
	create_java_class_def(+, +, -).

%%	ensure_normal_setup is det
%
%	ensure that the rdf store is ready
%	with protos.ttl and blockflow.ttl loaded
%
ensure_normal_setup :-
	flostate:setup_normally(true),
	!.
ensure_normal_setup :-
	rdf_load('blockflow.ttl', [format(turtle)]),
	rdf_load('protos.ttl', [format(turtle)]),
	flostate:asserta(setup_normally(true)).


%%	create_java_class_def(+Pkg:atom, +Name:atom, -URI:r)
%
create_java_class(Pkg, Name, URI) :-
	atom(Pkg),
	atom(Name),
	www_form_encode(Pkg, FormPkg),
	www_form_encode(Name, FormName),
	format(atom(URI),
	       'http://www.friendularity.org/ontology/flo#~w.~w',
	       [FormPkg, FormName]),
	assert_rdf_default(URI, rdf:type, flo:'JavaBinding'),
	assert_rdf_default(URI, flo:javaName, literal(Name)),
	assert_rdf_default(URI, flo:javaPackage, literal(Pkg)).

%%	create_connection(+LHS:terminal, +RHS:terminal) is det
%
%	add a connection
%
%	@arg LHS a term of form terminal(Name, Parm)
%	@arg RHS a term of form terminal(Name, Parm)
%
create_connection(terminal(LHName, LHParm), terminal(RHName, RHParm)) :-
	atom(LHName),
	atom(LHParm),
	atom(RHName),
	atom(RHParm),
	output_terminal(LHName, LHParm, LHBlockNode, LHParmNode),
	input_terminal(RHName, RHParm, RHBlockNode, RHParmNode),
	rdf(LHParmNode, flo:name, literal(LHParm)),
	rdf(RHParmNode, flo:name, literal(RHParm)),
	rdf_bnode(ConnName),
	assert_rdf_default(ConnName, rdf:type, flo:'Connection'),
	assert_rdf_default(ConnName, flo:rhsParm, literal(RHParm)),
	assert_rdf_default(ConnName, flo:rhs,  RHBlockNode),
	assert_rdf_default(ConnName, flo:lhsParm, literal(LHParm)),
	assert_rdf_default(ConnName, flo:lhs, LHBlockNode).

%%	create_named_block(+Name:atom, +Type:atom) is nondet
%
%	create a block named this
%
%	fails silently if Type is unknown or Name or Type arent atoms
%
%	@arg Name the name of the block
%	@arg Type the type of the block (name from excel)
%
create_named_block(Name, Type) :-
	atom(Name),
	atom(Type),
	rdf_global_id(flo:Type, TypeNodeName),
	% make sure the type really exists
	rdf(TypeNodeName, rdf:type, flo:'BlockType'),
	rdf_global_id(flo:Name, BlockNode),
	assert_rdf_default(BlockNode, rdf:type, flo:'Block'),
	assert_rdf_default(BlockNode, flo:hasType, TypeNodeName).

%%	create_anonymous_block(+Type:atom, -Name:atom) is nondet
%
%	create an anonymous block ,return the name
%
%	fails silently if Type is invalid
%
create_anonymous_block(Type, Name) :-
	ethel_name(Type, Name),
	create_named_block(Name, Type).

%%	create_publish_node(+Name:terminal, +Terminal:codes) is det
%
%	Create a publish node
%
%	@arg Name   the name to publish in ImageStreamBroker
%	@arg Terminal a terminal of form terminal(Name:codes,
%	Parm:codes)
%
create_publish_node(Name, terminal(Name, Parm)) :-
	is_list(Name),
	is_list(Parm),
	atom_codes(AName, Name),
	atom_codes(AParm, Parm),
	output_terminal(AName, AParm, BlockNode, ParmNode).
% TODO - finished here



%%	input_terminal(+Name:atom, +Parm:atom, -BlockNode:resource,
%	-ParmNode:resource) is nondet
%
%	fails silently if it can't find the block
%	or the terminal
%
input_terminal(Name, Parm, BlockNode, ParmNode) :-
	atom(Name),
	atom(Parm),
	rdf_global_id(flo:Name, BlockNode),
	rdf(BlockNode, flo:hasType, BlockType),
	rdf(BlockType, flo:requiresInput, ParmNode),
	rdf(ParmNode, flo:name, literal(Parm)).
input_terminal(Name, Parm, BlockNode, ParmNode) :-
	atom(Name),
	atom(Parm),
	rdf_global_id(flo:Name, BlockNode),
	rdf(BlockNode, flo:hasType, BlockType),
	rdf(BlockType, flo:optionalInput, ParmNode),
	rdf(ParmNode, flo:name, literal(Parm)).

%%	output_terminal(+Name:atom, +Parm:atom, -BlockNode:resource,
%	-ParmNode:resource) is nondet
%
%	fails silently if it can't find the block
%	or the terminal
%
output_terminal(Name, Parm, BlockNode, ParmNode) :-
	atom(Name),
	atom(Parm),
	rdf_global_id(flo:Name, BlockNode),
	rdf(BlockNode, flo:hasType, BlockType),
	rdf(BlockType, flo:hasOutput, ParmNode),
	rdf(ParmNode, flo:name, literal(Parm)).

%%	ethel_name(+Type:atom, -Name:atom) is det
%
%	return an atom gensym name for an anonymous block
%
ethel_name(Type, Name) :-
	uuid(R),
	format(atom(Name), 'block~w~w', [Type, R]).

:- dynamic flostate:default_graph/1.

%%	set_current_graph_name(+Graph:atom) is det
%
%	set the graph name used by assert_rdf_default
%
set_current_graph_name(Graph) :-
	flostate:asserta(default_graph(Graph)).

%%	assert_rdf_default(+S:r, +P:r, +O:o) is det
%
%	Assert a triple into the 'current' graph
%
assert_rdf_default(S, P, O) :-
	flostate:default_graph(Graph),
	rdf_assert(S, P, O, Graph).

%%	open_terminal(+Name:atom, -Parm:atom) is nondet
%
%	return the next open parameter if there is one
%
open_terminal(Name, Parm) :-
	atom(Name),
	atom(Parm),
	input_terminal(Name, Parm, BlockNode, ParmNode),
	not_connected_input(BlockNode, ParmNode).
open_terminal(Name, Parm) :-
	atom(Name),
	atom(Parm),
	output_terminal(Name, Parm, BlockNode, ParmNode),
	not_connected_output(BlockNode, ParmNode).

not_connected_input(BlockNode, ParmNode) :-
	rdf(ConnName, flo:rhs,  BlockNode),
        rdf(ConnName, flo:rhsParm, ParmNode),
	!, fail.
not_connected_input(_, _).

not_connected_output(BlockNode, ParmNode) :-
	rdf(ConnName, flo:rhs,  BlockNode),
        rdf(ConnName, flo:rhsParm, ParmNode),
	!, fail.
not_connected_output(_, _).

:- ensure_normal_setup.
