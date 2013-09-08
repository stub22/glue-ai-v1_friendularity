:- module(converter, [convert/1]).
/** <module> Non automagic code for making the rdf

*/
:- use_module(floblocks).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
% :- use_module(library(http/http_ssl_plugin)).

:- rdf_register_prefix(flo, 'http://www.friendularity.org/ontology/flo#').


:- rdf_meta assert_rdf_list(o, r, r, +, r, +).

/*
how to get csv from googledocs

https://docs.google.com/spreadsheet/ccc?key=0AkhpumfMtW6DdE5adlZjeHk4OWZwMGhGM1p3SzNYWFE&output=csv&range=A1:AC99
*/
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).

cert_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error) :-
        format(user_error, 'Accepting certificate~n', []).

google_proto_ss('https://docs.google.com/spreadsheet/ccc?key=0AkhpumfMtW6DdE5adlZjeHk4OWZwMGhGM1p3SzNYWFE&output=csv&range=A1:AC99').

google_unsecure_proto_ss('http://docs.google.com/spreadsheet/ccc?key=0AkhpumfMtW6DdE5adlZjeHk4OWZwMGhGM1p3SzNYWFE&output=csv&range=A1:AC99').

get_google_sheet :-
	google_proto_ss(HTTPS_url),
        http_open(HTTPS_url, In,
                  [ cert_verify_hook(cert_verify)
                  ]),
	read_stream_to_codes(In, Codes),
	close(In),
	format('~s', [Codes]),
	open('fromgoogle.csv', write, Out, [encoding(octet)]),
	format(Out, '~s', [Codes]),
	close(Out).

unsecure_get_google_sheet :-
	google_unsecure_proto_ss(HTTP_url),
        http_open(HTTP_url, In, [cert_verify_hook(cert_verify)]),
	read_stream_to_codes(In, Codes),
	close(In),
	format('~s', [Codes]),
	open('fromgoogle.csv', write, Out, [encoding(octet)]),
	format(Out, '~s', [Codes]),
	close(Out).



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
	rdf_assert(HGlobal, flo:visualStyle, literal(Style)),
	inputs(H, InputList),
	input_types(H, InputTypeList),
	assert_rdf_list(HGlobal, flo:inputFor, flo:name, InputList,
					     flo:dataType, InputTypeList),
	outputs(H, OutputList),
	output_types(H, OutputTypeList),
	assert_rdf_list(HGlobal, flo:outputFor,
			flo:name, OutputList,
			flo:dataType, OutputTypeList),
	(   image_name(H, ImageName) ->
	    rdf_assert(HGlobal, flo:imageResource, literal(ImageName))
	;
	    true),
	(   prototype_coordinates(H, X, Y) ->
	    rdf_assert(HGlobal, flo:defaultPrototypeCoordinateX, literal(X)),
	    rdf_assert(HGlobal, flo:defaultPrototypeCoordinateY, literal(Y))
	;
	    true),
	add_rdf(T).

assert_rdf_list(_, _, _, [], _, _).
assert_rdf_list(Subj, PredLinkingSubj, Type1, [required(H1)|T1], Type2, [H2|T2]) :- !,
	rdf_bnode(BNode),
	rdf_assert(BNode, rdf:type, flo:'BlockInput'),
	rdf_assert(BNode, PredLinkingSubj, Subj),
	rdf_assert(BNode, Type1, literal(H1)),
	rdf_global_id(flo:H2, H2Global),
	rdf_assert(BNode, Type2, H2Global),
	rdf_assert(Subj, flo:requiresInput, BNode),
	assert_rdf_list(Subj, PredLinkingSubj, Type1, T1, Type2, T2).
assert_rdf_list(Subj, PredLinkingSubj, Type1, [optional(H1)|T1], Type2, [H2|T2]) :- !,
	rdf_bnode(BNode),
	rdf_assert(BNode, rdf:type, flo:'BlockInput'),
	rdf_assert(BNode, PredLinkingSubj, Subj),
	rdf_assert(BNode, Type1, literal(H1)),
	rdf_global_id(flo:H2, H2Global),
	rdf_assert(BNode, Type2, H2Global),
	rdf_assert(Subj, flo:optionalInput, BNode),
	assert_rdf_list(Subj, PredLinkingSubj, Type1, T1, Type2, T2).
assert_rdf_list(Subj, PredLinkingSubj, Type1, [H1|T1], Type2, [H2|T2]) :-
	rdf_bnode(BNode),
	rdf_assert(BNode, rdf:type, flo:'BlockOutput'),
	rdf_assert(BNode, PredLinkingSubj, Subj),
	rdf_assert(BNode, Type1, literal(H1)),
	rdf_global_id(flo:H2, H2Global),
	rdf_assert(BNode, Type2, H2Global),
	rdf_assert(Subj, flo:hasOutput, BNode),
	assert_rdf_list(Subj, PredLinkingSubj, Type1, T1, Type2, T2).


write_rdf(Outfile) :-
	rdf_save_turtle(Outfile, []).

