:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)).
:- use_module(library(when)).
:- dynamic(tr/3).
:- dynamic(defaultGraph/1).

read_turtle_file(File) :-
    rdf_load(File, [format(turtle)]),
    forall(rdf(S, P, O),asserta(tr(S,P,O))),
    absolute_file_name(File,Abs),
    atom_concat('file://',Abs,DefaultGraph),
    asserta(defaultGraph(DefaultGraph)),
    rdf_retractall(_, _, _).

write_turtle_file(File) :-
    forall(
        quad(G, S, P, O), (
            ( G = 'urn:deo:defaultgraph' ->
                defaultGraph(DG) ,
                rdf_assert(S, P, O, DG) ;
                rdf_assert(S, P, O, G)
            )
        )
    ),
    rdf_save_trig(File, []).

policyClass('http://www.w3.org/ns/odrl/2/Set') .
policyClass('http://www.w3.org/ns/odrl/2/Offer') .
policyClass('http://www.w3.org/ns/odrl/2/Agreement') .

policyType('http://www.w3.org/ns/odrl/2/permission') .
policyType('http://www.w3.org/ns/odrl/2/prohibition') .
policyType('http://www.w3.org/ns/odrl/2/obligation') .

hasBody(Policy,Type,Body) :-
    tr(Policy,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',What) ,
    policyClass(What) ,
    policyType(Type) ,
    tr(Policy,Type,Body) .

bodyWithConstraint(Body) :-
    tr(Body,'http://www.w3.org/ns/odrl/2/constraint',_) .

bodyWithoutConstraint(Body) :-
    tr(Body,'http://www.w3.org/ns/odrl/2/target',_) ,
    \+ bodyWithConstraint(Body) .

% begin constaints

condition(Body,Subject) :-
    tr(Body,'http://www.w3.org/ns/odrl/2/constraint',Constraint) ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/operator',Op^^_) ,
    Op = "gt" ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/leftOperand',Predicate) ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/rightOperand',Required^^_) ,
    tr(Subject,Predicate,Value^^_) ,
    Value @> Required .

condition(Body,Subject) :-
    tr(Body,'http://www.w3.org/ns/odrl/2/constraint',Constraint) ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/operator',Op^^_) ,
    Op = "lt" ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/leftOperand',Predicate) ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/rightOperand',Required^^_) ,
    tr(Subject,Predicate,Value^^_) ,
    Value @< Required .

condition(Body,Subject) :-
    tr(Body,'http://www.w3.org/ns/odrl/2/constraint',Constraint) ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/operator',Op^^_) ,
    Op = "gteq" ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/leftOperand',Predicate) ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/rightOperand',Required^^_) ,
    tr(Subject,Predicate,Value^^_) ,
    Value @>= Required .

condition(Body,Subject) :-
    tr(Body,'http://www.w3.org/ns/odrl/2/constraint',Constraint) ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/operator',Op^^_) ,
    Op = "lteq" ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/leftOperand',Predicate) ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/rightOperand',Required^^_) ,
    tr(Subject,Predicate,Value^^_) ,
    Value @=< Required .

condition(Body,Subject) :-
    tr(Body,'http://www.w3.org/ns/odrl/2/constraint',Constraint) ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/operator',Op^^_) ,
    Op = "eq" ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/leftOperand',Predicate) ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/rightOperand',Required^^_) ,
    tr(Subject,Predicate,Value^^_) ,
    Value =@= Required .

condition(Body,Subject) :-
    tr(Body,'http://www.w3.org/ns/odrl/2/constraint',Constraint) ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/operator',Op^^_) ,
    Op = "neq" ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/leftOperand',Predicate) ,
    tr(Constraint,'http://www.w3.org/ns/odrl/2/rightOperand',Required^^_) ,
    tr(Subject,Predicate,Value^^_) ,
    Value \=@= Required .

% end constraints

valid_constraint(Body) :-
    bodyWithoutConstraint(Body) .

valid_constraint(Body) :-
    bodyWithConstraint(Body) ,
    tr(Body,'http://www.w3.org/ns/odrl/2/assignee',Assignee) , 
    condition(Body,Assignee) .

permission(Policy,Target,Assigner,Assignee,Action) :-
    hasBody(Policy,Type,Body) ,
    valid_constraint(Body) ,
    Type = 'http://www.w3.org/ns/odrl/2/permission' ,
    tr(Body,'http://www.w3.org/ns/odrl/2/assigner',Assigner) ,
    tr(Body,'http://www.w3.org/ns/odrl/2/assignee',Assignee) ,
    tr(Body,'http://www.w3.org/ns/odrl/2/action',Action) ,
    tr(Body,'http://www.w3.org/ns/odrl/2/target',Target) .

prohibition(Policy,Target,Assigner,Assignee,Action) :-
    hasBody(Policy,Type,Body) ,
    valid_constraint(Body) ,
    Type = 'http://www.w3.org/ns/odrl/2/prohibition' ,
    tr(Body,'http://www.w3.org/ns/odrl/2/assigner',Assigner) ,
    tr(Body,'http://www.w3.org/ns/odrl/2/assignee',Assignee) ,
    tr(Body,'http://www.w3.org/ns/odrl/2/action',Action) ,
    tr(Body,'http://www.w3.org/ns/odrl/2/target',Target) .

obligation(Policy,Target,Assigner,Assignee,Action) :-
    hasBody(Policy,Type,Body) ,
    valid_constraint(Body) ,
    Type = 'http://www.w3.org/ns/odrl/2/obligation' ,
    tr(Body,'http://www.w3.org/ns/odrl/2/assigner',Assigner) ,
    tr(Body,'http://www.w3.org/ns/odrl/2/assignee',Assignee) ,
    tr(Body,'http://www.w3.org/ns/odrl/2/action',Action) ,
    tr(Body,'http://www.w3.org/ns/odrl/2/target',Target) .

quad('urn:deo:defaultgraph','urn:deo:box','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','urn:deo:ModalOperator') .

% permission boxes

quad('urn:deo:defaultgraph',ROI,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2000/10/swap/log#onNegativeSurface') :-
    permission(Policy,_,_,_,_) ,
    atom_concat(Policy,'_outer',ROI) .

quad(ROI,Policy,'urn:deo:box',RII) :-
    permission(Policy,_,_,_,_) ,
    atom_concat(Policy,'_outer',ROI) ,
    atom_concat(Policy,'_inner',RII) .

quad(ROI,RII,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2000/10/swap/log#onNegativeSurface') :-
    permission(Policy,_,_,_,_) ,
    atom_concat(Policy,'_outer',ROI) ,
    atom_concat(Policy,'_inner',RII) .

quad(RII,Assignee,Action,Target) :-
    permission(Policy,Target,_,Assignee,Action) ,
    atom_concat(Policy,'_inner',RII) .

% prohibition boxes

quad('urn:deo:defaultgraph',Policy,'urn:deo:box',ROI) :-
    prohibition(Policy,_,_,_,_) ,
    atom_concat(Policy,'_outer',ROI) .

quad(ROI,RII,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2000/10/swap/log#onNegativeSurface') :-
    prohibition(Policy,_,_,_,_) ,
    atom_concat(Policy,'_outer',ROI),
    atom_concat(Policy,'_inner',RII) .

quad(RII,Assignee,Action,Target) :-
    prohibition(Policy,Target,_,Assignee,Action) ,
    atom_concat(Policy,'_inner',RII) .

% obligation boxes

quad('urn:deo:defaultgraph',Policy,'urn:deo:box',RI) :- 
    obligation(Policy,_,_,_,_) ,
    atom_concat(Policy,'_outer',RI) .

quad(RI,Assignee,Action,Target) :-
    obligation(Policy,Target,_,Assignee,Action) ,
    atom_concat(Policy,'_outer',RI) .

run :-
    read_turtle_file('data/policy1.ttl') ,
    write_turtle_file('example5.trig') .