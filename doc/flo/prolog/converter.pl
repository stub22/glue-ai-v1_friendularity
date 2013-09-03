:- module(converter, [convert/1]).
/** <module> Non automagic code for making the rdf

*/
:- use_module(floblocks).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/rdfs)).

:- rdf_register_prefix(flo, 'http://www.friendularity.org/ontology/flo#').


:- rdf_meta assert_rdf_list(o, r, r, +, r, +).


convert(Outfile) :-
	rdf_retractall(_, _, _),
	setof(BlockType, Section^section(BlockType, Section), BlockTypes),
	add_rdf(BlockTypes),
	write_rdf(Outfile).

add_rdf([]).
add_rdf([H|T]) :-
	writeln(H),
	rdf_global_id(flo:H, HGlobal),
	rdf_assert(HGlobal, rdf:type, flo:'BlockType'),
	subsection(H, SubSection),
	rdf_assert(HGlobal, flo:subsection, literal(SubSection)),
	section(H, Section),
	rdf_assert(HGlobal, flo:section, literal(Section)),
	description(H, Desc),
	rdf_assert(HGlobal, rdfs:comment, literal(Desc)),
	rdf_assert(HGlobal, rdfs:label, literal(H)),
	visual_style(H, Style),
	rdf_assert(HGlobal, flo:visual_style, literal(Style)),
	inputs(H, InputList),
	input_types(H, InputTypeList),
	assert_rdf_list(HGlobal, flo:inputFor, flo:inputName, InputList,
					     flo:inputType, InputTypeList),
	outputs(H, OutputList),
	output_types(H, OutputTypeList),
	assert_rdf_list(HGlobal, flo:outputFor,
			flo:outputName, OutputList,
			flo:outputType, OutputTypeList),
	(   image_name(H, ImageName) ->
	    rdf_assert(HGlobal, flo:imageResource, literal(ImageName))
	;
	    true),
	(   prototype_coordinates(H, X, Y) ->
	    rdf_assert(HGlobal, flo:prototypeCoordinateX, literal(X)),
	    rdf_assert(HGlobal, flo:prototypeCoordinateY, literal(Y))
	;
	    true),
	add_rdf(T).

assert_rdf_list(_, _, _, [], _, _).
assert_rdf_list(Subj, PredLinkingSubj, Type1, [required(H1)|T1], Type2, [H2|T2]) :- !,
	rdf_bnode(BNode),
	rdf_assert(BNode, PredLinkingSubj, Subj),
	rdf_assert(BNode, Type1, literal(H1)),
	rdf_assert(BNode, Type2, literal(H2)),
	rdf_assert(Subj, flo:requiresInput, BNode),
	assert_rdf_list(Subj, PredLinkingSubj, Type1, T1, Type2, T2).
assert_rdf_list(Subj, PredLinkingSubj, Type1, [optional(H1)|T1], Type2, [H2|T2]) :- !,
	rdf_bnode(BNode),
	rdf_assert(BNode, PredLinkingSubj, Subj),
	rdf_assert(BNode, Type1, literal(H1)),
	rdf_assert(BNode, Type2, literal(H2)),
	rdf_assert(Subj, flo:optionalInput, BNode),
	assert_rdf_list(Subj, PredLinkingSubj, Type1, T1, Type2, T2).
assert_rdf_list(Subj, PredLinkingSubj, Type1, [H1|T1], Type2, [H2|T2]) :-
	rdf_bnode(BNode),
	rdf_assert(BNode, PredLinkingSubj, Subj),
	rdf_assert(BNode, Type1, literal(H1)),
	rdf_assert(BNode, Type2, literal(H2)),
	rdf_assert(Subj, flo:optionalInput, BNode),
	assert_rdf_list(Subj, PredLinkingSubj, Type1, T1, Type2, T2).


write_rdf(Outfile) :-
	rdf_save_turtle(Outfile, []).

