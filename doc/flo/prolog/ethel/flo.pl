:- module(flo, [ensure_normal_setup/0,
	        create_connection/2,
	       set_current_graph_name/1,
	       create_named_block/2,
	       create_anonymous_block/2]).
/** <module> Predicates for manipulating the Flo rdf language

*/

:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uuid)).

:- rdf_register_prefix(flo, 'http://www.friendularity.org/ontology/flo#').
:- dynamic flostate:setup_normally/1.

ensure_normal_setup :-
	flostate:setup_normally(true),
	!.
ensure_normal_setup :-
	rdf_load('blockflow.ttl', [format(turtle)]),
	rdf_load('protos.ttl', [format(turtle)]),
	flostate:asserta(setup_normally(true)).

create_connection(terminal(LHName, LHParm), terminal(RHName, RHParm)) :-
	output_terminal(LHName, LHParm, LHBlockNode, LHParmNode),
	input_terminal(RHName, RHParm, RHBlockNode, RHParmNode),
	rdf_bnode(ConnName),
	assert_rdf_default(ConnName, rdf:type, flo:'Connection'),
	assert_rdf_default(ConnName, flo:rhsParm, RHParmNode),
	assert_rdf_default(ConnName, flo:rhs,  RHBlockNode),
	assert_rdf_default(ConnName, flo:lhsParm, LHParmNode),
	assert_rdf_default(ConnName, flo:lhs, LHBlockNode).

create_named_block(Name, Type) :-
	rdf_global_id(flo:Type, TypeNodeName),
	rdf(TypeNodeName, rdf:type, flo:'BlockType'),
	rdf_global_id(flo:Name, BlockNode),
	assert_rdf_default(BlockNode, rdf:type, TypeNodeName).

create_anonymous_block(Type, Name) :-
	ethel_name(Type, Name),
	create_named_block(Name, Type).


input_terminal(Name, Parm, BlockNode, ParmNode) :-
	rdf_global_id(flo:Name, BlockNode),
	rdf(ParmNode, flo:hasType, BlockType),
	rdf(BlockType, flo:requiresInput, ParmNode),
	rdf(ParmNode, flo:name, Parm).
input_terminal(Name, Parm, BlockNode, ParmNode) :-
	rdf_global_id(flo:Name, BlockNode),
	rdf(ParmNode, flo:hasType, BlockType),
	rdf(BlockType, flo:optionalInput, ParmNode),
	rdf(ParmNode, flo:name, Parm).

output_terminal(Name, Parm, BlockNode, ParmNode) :-
	rdf_global_id(flo:Name, BlockNode),
	rdf(ParmNode, flo:hasType, BlockType),
	rdf(BlockType, flo:hasOutput, ParmNode),
	rdf(ParmNode, flo:name, Parm).

ethel_name(Type, Name) :-
	uuid(R),
	format(atom(Name), 'block~w~w', [Type, R]).

:- dynamic flostate:default_graph/1.

set_current_graph_name(Graph) :-
	flostate:asserta(default_graph(Graph)).

assert_rdf_default(S, P, O) :-
	flostate:default_graph(Graph),
	rdf_assert(S, P, O, Graph).

:- ensure_normal_setup.
