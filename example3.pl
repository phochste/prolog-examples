:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)).
:- use_module(library(when)).
:- dynamic(tr/3).

read_turtle_file(File) :-
    rdf_load(File, [format(turtle)]),
    forall(rdf(S, P, O),asserta(tr(S,P,O))),
    rdf_retractall(_, _, _).

tr(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',B) :-
    tr(A,'http://www.w3.org/2000/01/rdf-schema#subClassOf',B) ,
    tr(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',A) .

write_turtle_file(File) :-
    rdf_retractall(_, _, _),  % Clear RDF store before writing
    forall(tr(S, P, O), rdf_assert(S, P, O)),
    rdf_save_turtle(File, []).

run :-
    read_turtle_file('data/demo.ttl') ,
    write_turtle_file('example3.ttl') .