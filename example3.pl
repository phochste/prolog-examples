:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)).
:- use_module(library(when)).
:- dynamic(tr/3).

read_turtle_file(File) :-
    rdf_load(File, [format(turtle)]),
    forall(rdf(S, P, O),asserta(tr(S,P,O))),
    rdf_retractall(_, _, _).

tr(S,'http://example.org/ns#nameLength',L) :-
    tr(S,'http://example.org/ns#name',N^^_) ,
    string_length(N,L).

write_turtle_file(File) :-
    rdf_retractall(_, _, _),  % Clear RDF store before writing
    forall(tr(S, P, O), rdf_assert(S, P, O)),
    rdf_save_turtle(File, []).

run :-
    read_turtle_file('data/demo.ttl') ,
    write_turtle_file('example3.ttl') .