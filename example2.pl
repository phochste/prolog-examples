:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)).
:- use_module(library(when)).
:- dynamic(tr/3).

read_turtle_file(File) :-
    rdf_load(File, [format(turtle)]),
    forall(rdf(S, P, O),asserta(tr(S,P,O))),
    rdf_retractall(_, _, _).

tr(X,'http://example.org/ns#parent',Y) :-
    tr(X,'http://example.org/ns#mother',Y) . 

tr(X,'http://example.org/ns#parent',Y) :-
    tr(X,'http://example.org/ns#father',Y) . 

write_turtle_file(File) :-
    rdf_retractall(_, _, _),  % Clear RDF store before writing
    forall(tr(S, P, O), rdf_assert(S, P, O)),
    rdf_save_turtle(File, []).

run :-
    read_turtle_file('data/demo2.ttl') ,
    write_turtle_file('example2.ttl') .