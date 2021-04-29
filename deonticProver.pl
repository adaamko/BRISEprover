/* deonticProver.pl
*/

/* operator definitions etc */
  :- op(400,fy,neg).
  :- op(500,xfy,and).
  :- op(600,xfy,or).
  :- op(700,xfy,->).
  :- op(800,xfy,=>).
  :- op(900,xfy,:).
  :- op(900,xfy,beats).

  :- use_module(library(lists)).

/* additional operators used for formulae:
     obl(.,.), rec(.,.), for(.,.), pero(.,.), perf(.,.)
   the latter two only in the lists of Srauta assumptions.
   UPDATE 2.0: the operators are given as input
*/


/* load parts for prettyprinting and for preprocessing
*/
:- dynamic(conflicting_assumptions/2). % the predicate is asserted in preprocessing.pl
:- ensure_loaded([prettyprinting]).
:- ensure_loaded([preprocessing]).
:- ensure_loaded([attributes]).
/* load example formalisation
 * every Plangebiet has its own file
*/
:- multifile(bauland/3).
:- multifile(bauland_facts/3).
:- multifile(grundflaechen/3).
:- multifile(grundflaechen_facts/3).
:- multifile(grundflaechen_obligations/3).
:- multifile(fluchtlinien/3).
:- multifile(fluchtlinien_facts/3).
:- multifile(textliche_bestimmungen/3).
:- ensure_loaded([assumptionhandler]).
:- ensure_loaded([pd7602_new]).
:- ensure_loaded([pd7601]).
:- ensure_loaded([pd6963]).
:- ensure_loaded([bauordnung_new]).
:- ensure_loaded([icailexample]).

/* DATA STRUCTURE
   We're working on sequents.

   The predicate prove has as last argument a tree which will turn
   into the derivation
   All the assumptions, operators, structure of the operators etc is
   passed on in the prove predicate

   Nodes of the derivation tree contain information about the content
   of the node (the sequent), the rule, possibly indexed by the
   operator and some more formulae for the assumption rules.

   The modal formulae are given by
     modal(Operator, Subfml1, Subfml2)
   instead of Operator(Subfml1,Subfml2).
   This way we can keep the operator generic and have easy access to it.
*/

/*
Input:
- a formula
- a list of factual assumptions. Format:
  List of A1 and ... and AN -> B1 or ... or BM
- a list of deontic assumptions. Format: 
  List of modal(Op,A,B) or Norm:modal(Op,A,B) 
- a list of operators with their type (prohibition or obligation). Format:
  List of (Op,obl) or (Op,for)
- a superiority relation. Format: 
  List of (A beats B)
- a conflict relation. Format: 
  List of confl(Op1,Op2)
- a "P" relation. Format:
  List of nt(Op)
- an inclusion relation. Format: 
  List of Op1 -> Op2
*/

/* preprocessing:
- add at(.) to all the atoms
- saturate the factual assumptions
- make sure that every operator only has one type
- calculate the reflexive and transitive closure of the inclusion
  relation
- calculate the closure of the conflict relation under adding
  symmetry and the preimage of the inclusion relation
- calculate the closure of the P relation
- check that the superiority relation is not cyclic and return the
  cycles.
- For the MODERN VERSION (using conflict lists): construct and assert
  the conflict list for every deontic assumption.
*/

/* Pretty printing
   Done using DCGs; One version producing a latex source, one
   producing an html file with the explanation.
*/

/* Assumptions
   Have the form
   asmp(Facts, Deontic_assumptions, Op_characterisation, Relation)
   where Op_characterisation has the form
     ops(Operator_list, Inclusion_relation, Conflict_relation, P_relation)
*/


/* briseprover_local
   for running the prover locally
*/
briseprover_local(Fml, Facts, D_Assumptions, Sup_Relation, Ex_list, derivation) :-
    subtract(Ex_list,[standardoperators],Ex_list1),
    prove_online(Fml, Facts, D_Assumptions, Sup_Relation, [], [], []
		 , [], [standardoperators|Ex_list1], modern, derivability, 'output.tex'),!.
briseprover_local(Fml, Facts, D_Assumptions, Sup_Relation, Ex_list, explanation) :-
    subtract(Ex_list,[standardoperators],Ex_list1),
    explain_online(Fml, Facts, D_Assumptions, Sup_Relation, [], [], []
		 , [], [standardoperators|Ex_list1], modern, derivability, 'output.html'),!.

/* prove_online
   predicate to be called from the web interface
*/
prove_online(Fml, Facts, D_Assumptions, Sup_Relation, Operators,
	     Inclusions, Conflicts, P_list, [], Version, derivability, Filename) :-
    \+ member(test,Facts),
    prove_with_filename(Fml, Operators, Inclusions, Conflicts, P_list,
			Facts, D_Assumptions, Sup_Relation, Version, Filename).
% For adding the standard operators
prove_online(Fml, Facts, D_Assumptions, Sup_Relation, Operators,
	     Inclusions, Conflicts, P_list, [standardoperators], Version, derivability, Filename) :-
    prove_with_filename(Fml, [(obl,obl), (for,for), (per,obl)|Operators], Inclusions, [confl(obl,obl), confl(obl,per), confl(obl,for), confl(for,for), confl(for,per)|Conflicts], P_list,
			Facts, D_Assumptions, Sup_Relation, Version, Filename).
% for testing with the plandokumente
prove_online(Fml, Facts, D_Assumptions, Sup_Relation, Operators,
	     Inclusions, Conflicts, P_list, Ex_list, Version, derivability, Filename) :-
    subtract(Ex_list,[standardoperators],Ex_list1),
    phrase(added_facts(Facts,Ex_list1),New_Facts),
    phrase(added_assumptions(D_Assumptions,Ex_list1),New_D_Assumptions1),
    phrase(bauordnung(b),Bauordnung_assumptions),
    append(Bauordnung_assumptions,New_D_Assumptions1,New_D_Assumptions),
    prove_with_filename(Fml, [(obl,obl), (for,for), (per,obl)|Operators], Inclusions, [confl(obl,obl), confl(obl,per), confl(obl,for), confl(for,for), confl(for,per)|Conflicts], P_list,
			New_Facts, New_D_Assumptions, Sup_Relation,
			Version, Filename).
% For compliance checks
prove_online(Fml, Facts, D_Assumptions, Sup_Relation, Operators,
	     Inclusions, Conflicts, P_list, _, Version, compliance, Filename) :-
    include_type(obl,Operators,Obligations),
    include_type(for,Operators,Prohibitions),
    preprocess(Version,asmp(Facts, D_Assumptions, ops(Operators, Inclusions
					      , Conflicts, P_list),
		    Sup_Relation), Ass_Processed,_),
    modalised(Fml,Fml1),
    added_at(Fml1,Fml2),
    maplist(apply_op(Fml2,Fml2), Prohibitions, Proh_list),
    maplist(apply_op(neg Fml2,Fml2), Obligations, Obl_list),
    append(Obl_list,Proh_list,List),
    big_disjunction(List,Disj),
    (prove(Version,Ass_Processed, seq([],[Disj]), Derivation)
    ;
    nonderivable_statement(Derivation)),!,
    phrase(pp_compliance_output(latex,Ass_Processed,Fml2,Disj,Derivation),L),
    atomic_list_concat(L,L1),
    open(Filename,write,Stream),
    write(Stream,L1),
    close(Stream),!.


/* explain_online
   for outputting an html explanation.
*/
explain_online(Fml, Facts, D_Assumptions, Sup_Relation, Operators,
	     Inclusions, Conflicts, P_list, [], Version, derivability, Filename) :-
    explain_with_filename(Fml, Operators, Inclusions, Conflicts, P_list,
			Facts, D_Assumptions, Sup_Relation,
			Version, Filename).
explain_online(Fml, Facts, D_Assumptions, Sup_Relation, Operators,
	     Inclusions, Conflicts, P_list, [standardoperators], Version, derivability, Filename) :-
    explain_with_filename(Fml, [(obl,obl), (for,for), (per,obl)|Operators], Inclusions, [confl(obl,obl), confl(obl,per), confl(obl,for), confl(for,for), confl(for,per)|Conflicts], P_list,
			Facts, D_Assumptions, Sup_Relation,
			Version, Filename).
explain_online(Fml, Facts, D_Assumptions, Sup_Relation, Operators,
	     Inclusions, Conflicts, P_list, Ex_list, Version, derivability, Filename) :-
    subtract(Ex_list,[standardoperators],Ex_list1),
    phrase(added_facts(Facts,Ex_list1),New_Facts),
    phrase(added_assumptions(D_Assumptions,Ex_list1),New_D_Assumptions1),
    phrase(bauordnung(b),Bauordnung_assumptions),
    append(Bauordnung_assumptions,New_D_Assumptions1,New_D_Assumptions),
    explain_with_filename(Fml, [(obl,obl), (for,for), (per,obl)|Operators], Inclusions, [confl(obl,obl), confl(obl,per), confl(obl,for), confl(for,for), confl(for,per)|Conflicts], P_list,
			New_Facts, New_D_Assumptions, Sup_Relation,
			Version, Filename).

/* for testing
   explain_test
*/
explain_test(Fml) :-
    explain_online(Fml,[],[],[],[],[],[],[],[plangebiet(icail)],modern,derivability,'test.html'),!.
/* prove_test
*/
prove_test(Fml) :-
    prove_online(Fml,[],[],[],[],[],[],[],[plangebiet(icail)],modern,derivability,'test.tex'),!.


/* apply_op
   (for compliance check)
*/
apply_op(Fml1,Fml2,Op,modal(Op,Fml1,Fml2)).


/* include_type /3
   true if every operator of type Type from List1 is included in List2
   (for compliance check)
*/
include_type(_,[],[]).
include_type(Type,[(Op,Type)|Tail1],[Op|Tail2]) :-
    include_type(Type,Tail1,Tail2).
include_type(Type,[_|Tail1],Tail2) :-
    include_type(Type,Tail1,Tail2).


/* big_disjunction
   true if the disjunction of the things in the first argument is the
   second argument
   (For compliance check)
*/
big_disjunction([],false).
big_disjunction([Fml],Fml).
big_disjunction([Fml|Tail],Fml or Fml2) :-
    big_disjunction(Tail,Fml2).


/* prove_with_filename
 */
/* TODO: add the input check to preprocess
   i.e.: get Output from preprocess
         pass Output to pp_Output
         Format: empty list if input is fine, then print "Input ok!" (or nothing)
         otherwise list of [formula_error(Fml),
         facts_error(List_of_bad_facts),
         deontic_assumptions_error(List_of_bad_assumptions),
         operator_error(List_of_bad_operators), etc]

   [ ] retractall(conflicting_assumptions(_,_) at the end?
*/
prove_with_filename(Fml, Operators, Op_Inclusions, Op_Conflicts,
		    Op_P, Facts, D_Assumptions, Sup_Relation, Version,
		    Filename
		   ) :-
    preprocess(Version,asmp(Facts, D_Assumptions, ops(Operators, Op_Inclusions
					      , Op_Conflicts, Op_P),
		    Sup_Relation), Ass_Processed,_),
    modalised(Fml,Fml1),
    added_at(Fml1,Fml2),!, % cut for efficiency
    (prove(Version,Ass_Processed, seq([],[Fml2]), Derivation)
    ;
    nonderivable_statement(Derivation)),!,
    phrase(pp_output(latex,Ass_Processed,Fml2,Derivation),L),
    atomic_list_concat(L,L1),
    open(Filename,write,Stream),
    write(Stream,L1),
    close(Stream),!.


/* explain_with_filename
   For outputting explanation
*/
explain_with_filename(Fml, Operators, Op_Inclusions, Op_Conflicts,
		    Op_P, Facts, D_Assumptions, Sup_Relation, Version,
		    Filename
		   ) :-
    preprocess(Version,asmp(Facts, D_Assumptions, ops(Operators, Op_Inclusions
					      , Op_Conflicts, Op_P),
		    Sup_Relation), Ass_Processed,_),
    modalised(Fml,Fml1),
    added_at(Fml1,Fml2),!, % cut for efficiency
    (prove(Version,Ass_Processed, seq([],[Fml2]), Derivation)
    ;
    nonderivable_statement(Derivation)),!,
    tree_vs_named_tree(Derivation,Derivation_named),
    phrase(pp_output(html,Ass_Processed,Fml2,Derivation_named),L),
    atomic_list_concat(L,L1),
    open(Filename,write,Stream),
    write(Stream,L1),
    close(Stream),!.


/* TESTING PREDICATES */

/* prove_test
   to test the prove predicate
*/
prove_test1(Fml,Version) :-
    modalised(Fml,Fml1),
    added_at(Fml1,Fml2),
    prove(Version,asmp([],[],
	       ops([(obl,obl)],[obl -> obl],[confl(obl,obl)],[]),[]),
	  seq([],[Fml2]),T),
    phrase(pp_output(screen,asmp([],[],
	       ops([(obl,obl)],[obl -> obl],[confl(obl,obl)],[]),[]),Fml2,T),L),
    atomic_list_concat(L,L1),
    write(L1).
prove_test2(Fml,Dass) :-
    modalised(Fml,Fml1),
    added_at(Fml1,Fml2),
    maplist(modalised,Dass,Dass1),
    maplist(added_at,Dass1,Dass2),
    retractall(conflicting_assumptions(_,_)),
    make_conflict_lists(asmp([],Dass2,
	       ops([(obl,obl)],[obl -> obl],[confl(obl,obl)],[]),[]), Dass2),!,
    prove(modern,asmp([],Dass2,
	       ops([(obl,obl)],[obl -> obl],[confl(obl,obl)],[]),[]),
	  seq([],[Fml2]),T),
    phrase(pp_output(screen,asmp([],Dass2,
	       ops([(obl,obl)],[obl -> obl],[confl(obl,obl)],[]),[]),Fml2,T),L),
    atomic_list_concat(L,L1),
    write(L1),!.

/*  END OF TESTING PREDICATES */



/* prove /4
   prove(Logic,Ass,Seq,Tree) is true if given the assumptions Ass, the
   sequent Seq is provable in Logic and the derivation tree is Tree.
 */

/* initial sequents */
prove(_,_, seq(Gamma, Delta),
      node(botL, seq([false],[]), seq(Gamma,Delta),[])) :-
    member(false, Gamma),!. % cut for efficiency
prove(_,_, seq(Gamma, Delta),
      node(topR, seq([],[true]), seq(Gamma,Delta),[])) :-
    member(true, Delta),!. % cut for efficiency
prove(_,_, seq(Gamma, Delta),
      node(init, seq([F],[F]), seq(Gamma,Delta),[])) :-
    member(F, Gamma), member(F, Delta),!. % cut for efficiency

/* factual assumptions */
prove(_,asmp(Facts,_,_,_), seq(Gamma,Delta), node(fact, seq(Sigma, Pi),
						seq(Gamma, Delta),[])) :-
    member(seq(Sigma,Pi), Facts),
    subset(Sigma,Gamma),
    subset(Pi,Delta),!. % cut for efficiency

/* Assumptions about measures */
/* For the general representation of measures as
 * measure(Type,Object,Value) with the corresponding
 * min_measure(Type,Object,Value) and max_measure(Type,Object,Value).
*/
% measure is not greater than max_measure:
prove(_,_, seq(Gamma,Delta),
      node(measurefact, seq([at(measure(Type,Object,N)),at(max_measure(Type,Object,M))],[]),
	   seq(Gamma, Delta), [])) :- 
    member(at(measure(Type,Object,N)),Gamma),
    member(at(max_measure(Type,Object,M)),Gamma),
    N > M,!.% cut for efficiency
% measure is not smaller than min_measure:
prove(_,_, seq(Gamma,Delta),
      node(measurefact, seq([at(measure(Type,Object,N)),at(min_measure(Type,Object,M))],[]),
	   seq(Gamma, Delta), [])) :- 
    member(at(measure(Type,Object,N)),Gamma),
    member(at(min_measure(Type,Object,M)),Gamma),
    N < M,!. % cut for efficiency
% min_measure is monotone:
prove(_,_, seq(Gamma,Delta),
      node(measurefact, seq([at(min_measure(Type,Object,N))],[at(min_measure(Type,Object,M))]),
	   seq(Gamma, Delta), [])) :- 
    member(at(min_measure(Type,Object,N)),Gamma),
    member(at(min_measure(Type,Object,M)),Delta),
    M =< N, !. % cut for efficiency
% max_measure is monotone:
prove(_,_, seq(Gamma,Delta),
      node(measurefact, seq([at(max_measure(Type,Object,N))],[at(max_measure(Type,Object,M))]),
	   seq(Gamma, Delta), [])) :- 
    member(at(max_measure(Type,Object,N)),Gamma),
    member(at(max_measure(Type,Object,M)),Delta),
    N =< M, !. % cut for efficiency
% min_measure is not larger than max_measure:
prove(_,_, seq(Gamma,Delta),
      node(measurefact, seq([at(min_measure(Type,Object,N)),at(max_measure(Type,Object,M))],[]),
	   seq(Gamma, Delta), [])) :- 
    member(at(min_measure(Type,Object,N)),Gamma),
    member(at(max_measure(Type,Object,M)),Gamma),
    N > M, !. % cut for efficiency
% measure implies max_measure:
prove(_,_, seq(Gamma,Delta),
      node(measurefact, seq([at(measure(Type,Object,N))],[at(max_measure(Type,Object,M))]),
	   seq(Gamma, Delta), [])) :- 
    member(at(measure(Type,Object,N)),Gamma),
    member(at(max_measure(Type,Object,M)),Delta),
    N =< M, !. % cut for efficiency
% measure implies min_measure:
prove(_,_, seq(Gamma,Delta),
      node(measurefact, seq([at(measure(Type,Object,N))],[at(min_measure(Type,Object,M))]),
	   seq(Gamma, Delta), [])) :- 
    member(at(measure(Type,Object,N)),Gamma),
    member(at(min_measure(Type,Object,M)),Delta),
    M =< N, !. % cut for efficiency
% measure is unique:
prove(_,_, seq(Gamma,Delta),
      node(measurefact, seq([at(measure(Type,Object,N)),at(measure(Type,Object,M))],[]),
	   seq(Gamma, Delta), [])) :- 
    member(at(measure(Type,Object,N)),Gamma),
    member(at(measure(Type,Object,M)),Gamma),
    N > M, !. % cut for efficiency

% assumptions about measuretriples in general
/* measure triples are defined in attributes.pl and consist of a
 * triple with names for measure, min_measure and max_measure
*/
% measure is not greater than max_measure:
prove(_,_, seq(Gamma,Delta),
      node(measurefact, seq([at(Fml),at(FmlMax)],[]),
	   seq(Gamma, Delta), [])) :- 
    member(at(Fml),Gamma),
    member(at(FmlMax),Gamma),
    Fml =.. [Measure,N|_],
    FmlMax =.. [MeasureMax,M|_],
    measuretriple(Measure,_,MeasureMax),
    N > M,!.% cut for efficiency
% measure is not smaller than min_measure:
prove(_,_, seq(Gamma,Delta),
      node(measurefact, seq([at(Fml),at(FmlMin)],[]),
	   seq(Gamma, Delta), [])) :- 
    member(at(Fml),Gamma),
    member(at(FmlMin),Gamma),
    Fml =.. [Measure,N|_],
    FmlMin =.. [MeasureMin,M|_],
    measuretriple(Measure,MeasureMin,_),
    N < M,!. % cut for efficiency
% min_measure is monotone:
prove(_,_, seq(Gamma,Delta),
      node(measurefact, seq([at(FmlMin1)],[at(FmlMin2)]),
	   seq(Gamma, Delta), [])) :- 
    member(at(FmlMin1),Gamma),
    member(at(FmlMin2),Delta),
    FmlMin1 =.. [MeasureMin,N|_],
    FmlMin2 =.. [MeasureMin,M|_],
    measuretriple(_,MeasureMin,_),
    M =< N, !. % cut for efficiency
% max_measure is monotone:
prove(_,_, seq(Gamma,Delta),
      node(measurefact, seq([at(FmlMax1)],[at(FmlMax2)]),
	   seq(Gamma, Delta), [])) :- 
    member(at(FmlMax1),Gamma),
    member(at(FmlMax2),Delta),
    FmlMax1 =.. [MeasureMax,N|_],
    FmlMax2 =.. [MeasureMax,M|_],
    measuretriple(_,_,MeasureMax),
    N =< M, !. % cut for efficiency
% min_measure is not larger than max_measure:
prove(_,_, seq(Gamma,Delta),
      node(measurefact, seq([at(FmlMin),at(FmlMax)],[]),
	   seq(Gamma, Delta), [])) :- 
    member(at(FmlMin),Gamma),
    member(at(FmlMax),Gamma),
    FmlMin =.. [MeasureMin,N|_],
    FmlMax =.. [MeasureMax,M|_],
    measuretriple(_,MeasureMin,MeasureMax),
    N > M, !. % cut for efficiency
% measure implies max_measure:
prove(_,_, seq(Gamma,Delta),
      node(measurefact, seq([at(Fml)],[at(FmlMax)]),
	   seq(Gamma, Delta), [])) :- 
    member(at(Fml),Gamma),
    member(at(FmlMax),Delta),
    Fml =.. [Measure,N|_],
    FmlMax =.. [MeasureMax,M|_],
    measuretriple(Measure,_,MeasureMax),
    N =< M, !. % cut for efficiency
% measure implies min_measure:
prove(_,_, seq(Gamma,Delta),
      node(measurefact, seq([at(Fml)],[at(FmlMin)]),
	   seq(Gamma, Delta), [])) :- 
    member(at(Fml),Gamma),
    member(at(FmlMin),Delta),
    Fml =.. [Measure,N|_],
    FmlMin =.. [MeasureMin,M|_],
    measuretriple(Measure,MeasureMin,_),
    M =< N, !. % cut for efficiency
% measure is unique:
prove(_,_, seq(Gamma,Delta),
      node(measurefact, seq([at(Fml1),at(Fml2)],[]),
	   seq(Gamma, Delta), [])) :- 
    member(at(Fml1),Gamma),
    member(at(Fml2),Gamma),
    Fml1 =.. [Measure,N|_],
    Fml2 =.. [Measure,M|_],
    measuretriple(Measure,_,_),
    N > M, !. % cut for efficiency

/* propositional rules */
/* non-branching rules first for efficiency*/
/* negation */
prove(L,Assumptions, seq(Gamma,Delta),
      node(negL, seq([neg A],[]), seq(Gamma,Delta), [T])) :-
    select(neg A, Gamma, Sigma),
    \+ member(A, Delta),
    \+ member(inv(A), Delta),!, % cut for invertibility
    prove(L,Assumptions, seq([inv(A)|Sigma], [A|Delta]), T),!.% cut for efficiency
prove(L,Assumptions, seq(Gamma,Delta),
      node(negR, seq([],[neg A]), seq(Gamma,Delta), [T])) :-
    select(neg A, Delta, Pi),
    \+ member(A, Gamma),
    \+ member(inv(A), Gamma),!, % cut for invertibility
    prove(L,Assumptions, seq([A|Gamma], [inv(A)|Pi]), T),!.% cut for efficiency

/* conjunction left */
prove(L,Assumptions, seq(Gamma, Delta),
      node(conjL, seq([A and B],[]), seq(Gamma,Delta), [T])) :-
    select(A and B, Gamma, Sigma),
    ((\+ member(A,Gamma), \+ member(inv(A),Gamma))
    ;(\+ member(B,Gamma), \+ member(inv(B),Gamma))),
    prove(L,Assumptions, seq([A,B|Sigma],Delta), T),!.% cut for efficiency

/* disjunction right */
prove(L,Assumptions, seq(Gamma, Delta),
      node(disjR, seq([],[A or B]), seq(Gamma,Delta), [T])) :-
    select(A or B, Delta, Pi),
    ((\+ member(A,Delta), \+ member(inv(A),Delta))
    ;(\+ member(B,Delta), \+ member(inv(B),Delta))),!, % cut for invertibility
    prove(L,Assumptions, seq(Gamma,[A,B|Pi]), T),!.% cut for efficiency

/* implication right */
prove(L,Assumptions, seq(Gamma, Delta),
      node(implR, seq([],[A -> B]), seq(Gamma,Delta), [T])) :-
    select(A -> B, Delta, Pi),
    ((\+ member(A,Gamma), \+ member(inv(A),Gamma))
    ;(\+ member(B,Delta), \+ member(inv(B),Delta))),!, % cut for invertibility
    prove(L,Assumptions, seq([A|Gamma],[B|Pi]), T),!.% cut for efficiency

/* branching rules */
/* conjunction right */
prove(L,Assumptions, seq(Gamma, Delta),
      node(conjR, seq([],[A and B]), seq(Gamma, Delta), [T1,T2])) :-
    select(A and B, Delta, Pi),
    \+ member(A, Delta), \+ member(inv(A), Delta),
    \+ member(B, Delta), \+ member(inv(B), Delta),!, % cut for invertibility
    prove(L,Assumptions, seq(Gamma, [A|Pi]), T1),
    prove(L,Assumptions, seq(Gamma, [B|Pi]), T2),!.% cut for efficiency

/* disjunction left */
prove(L,Assumptions, seq(Gamma, Delta),
      node(disjL, seq([A or B],[]), seq(Gamma, Delta), [T1,T2])) :-
    select(A or B, Gamma, Sigma),
    \+ member(A, Gamma), \+ member(inv(A), Gamma),
    \+ member(B, Gamma), \+ member(inv(B), Gamma),!, % cut for invertibility
    prove(L,Assumptions, seq([A|Sigma], Delta), T1),
    prove(L,Assumptions, seq([B|Sigma], Delta), T2),!.% cut for efficiency

/* implication left */
prove(L,Assumptions, seq(Gamma, Delta),
      node(implL, seq([A -> B],[]), seq(Gamma, Delta), [T1,T2])) :-
    select(A -> B, Gamma, Sigma),
    \+ member(A, Delta), \+ member(inv(A), Delta),
    \+ member(B, Gamma), \+ member(inv(B), Gamma),!, % cut for invertibility
    prove(L,Assumptions, seq([B|Sigma], Delta), T1),
    prove(L,Assumptions, seq(Sigma, [A|Delta]), T2),!.% cut for efficiency


/* deontic/modal rules */
/* monotonicity rule */
prove(L,Assumptions, seq(Gamma,Delta),
      node(mon(Op1,Op2), seq([modal(Op1,A,B)],[modal(Op2,C,D)]),
	   seq(Gamma,Delta), [T1, T2, T3] )) :-
    member(modal(Op1,A,B), Gamma),
    member(modal(Op2,C,D), Delta),
    implies(Assumptions,Op1,Op2),
    impl(Assumptions, Op1, Op2, A, C, Seq),
    prove(L,Assumptions, Seq, T1),
    prove(L,Assumptions, seq([B],[D]), T2),
    prove(L,Assumptions, seq([D],[B]), T3),!. % cut for efficiency

/* D rule */
prove(L,Assumptions, seq(Gamma,Delta),
      node(confl(Op1,Op2), seq([modal(Op1,A,B), modal(Op2,C,D)],[]),
	   seq(Gamma,Delta), [T1, T2, T3] )) :-
    member(modal(Op1,A,B), Gamma),
    member(modal(Op2,C,D), Gamma),
    conflicts(Assumptions, Op1, Op2),
    confl(Assumptions, Op1, Op2, A, C, Seq),
    prove(L,Assumptions, Seq, T1),
    prove(L,Assumptions, seq([B],[D]), T2),
    prove(L,Assumptions, seq([D],[B]), T3),!. % cut for efficiency

/* P rule */
/* NOTE: for operators with Op confl Op this already is covered by the
 * D rule above! */
prove(L,Assumptions, seq(Gamma,Delta),
      node(pRule(Op), seq([modal(Op,A,B)],[]),
	   seq(Gamma,Delta), [T] )) :-
    member(modal(Op,A,B), Gamma),
    nontrivial(Assumptions, Op),
    confl(Assumptions, Op, Op, A, A, Seq),
    prove(L,Assumptions, Seq, T),!. % cut for efficiency

/* assumption right rule */
/* classic version (for the specificity handling as in the DEON 2020 paper) */
prove(classic,asmp(Facts,D_ass_list,Op_char,Rel), seq(Gamma,Delta),
      node(asmpR(Op1,Assumption), seq([],[modal(Op1,A,B)]),
	   seq(Gamma,Delta), [T1,T2,T3,node(not_overruled(Assumption),Tree_list)]))
:-
    member(modal(Op1,A,B), Delta),
    % the formula modal(Op2,C,D) or Norm:modal(Op2,C,D) is in D_ass_list:
    member_norm(Assumption,Op2,C,D,D_ass_list),
    implies(asmp(Facts,D_ass_list,Op_char,Rel),Op2,Op1),
    % the assumption is applicable:
    prove(classic,asmp(Facts,D_ass_list,Op_char,Rel), seq([B],[D]), T1),
    % the condition is implied by the assumption:
    impl(asmp(Facts,D_ass_list,Op_char,Rel), Op2,Op1, C,A, Seq1),
    prove(classic,asmp(Facts,D_ass_list,Op_char,Rel), Seq1, T2),
    % there is no conflict with the P-axiom for Op1 if applicable:
    no_conflict_p(asmp(Facts,D_ass_list,Op_char,Rel),Op1,A,T3),
    % filter out the assumptions for conflicting operators:
    include(applicable_assumption(asmp(Facts,D_ass_list,Op_char,Rel),
			     modal(Op2,C,D)),
	    D_ass_list, Outer_list),
    not_overruled(r,asmp(Facts,D_ass_list,Op_char,Rel),
			  modal(Op1,A,B), Assumption, Outer_list,
	    Tree_list),!. % cut for efficiency

/* modern version (for the new way of specificity handling) */
prove(modern,asmp(Facts,D_ass_list,Op_char,Rel), seq(Gamma,Delta),
      node(asmpR(Op1,Assumption), seq([],[modal(Op1,A,B)]),
	   seq(Gamma,Delta), [T1,T2,T3,node(not_overruled(Assumption),Tree_list)]))
:-
    member(modal(Op1,A,B), Delta),
    % the formula modal(Op2,C,D) or Norm:modal(Op2,C,D) is in D_ass_list:
    member_norm(Assumption,Op2,C,D,D_ass_list),
    implies(asmp(Facts,D_ass_list,Op_char,Rel),Op2,Op1),
    % the assumption is applicable:
    prove(modern,asmp(Facts,D_ass_list,Op_char,Rel), seq([B],[D]), T1),
    % the condition is implied by the assumption:
    impl(asmp(Facts,D_ass_list,Op_char,Rel), Op2,Op1, C,A, Seq1),
    prove(modern,asmp(Facts,D_ass_list,Op_char,Rel), Seq1, T2),
    % there is no conflict with the P-axiom for Op1 if applicable:
    no_conflict_p_modern(asmp(Facts,D_ass_list,Op_char,Rel),Op1,A,T3),
    % filter out the assumptions for conflicting operators:
    % Retrieve the cached conflict list for Op2(C,D):
    conflicting_assumptions(modal(Op2,C,D),Outer_list),
    not_overruled_modern(r,asmp(Facts,D_ass_list,Op_char,Rel),
			  modal(Op1,A,B), Assumption, Outer_list,
	    Tree_list),!. % cut for efficiency
	    
/* assumption left rule */
/* classic version (for the specificity handling as in the DEON 2020 paper) */
prove(classic,asmp(Facts,D_ass_list,Op_char,Rel), seq(Gamma,Delta),
      node(asmpL(Op1,Assumption), seq([modal(Op1,A,B)],[]),
	   seq(Gamma,Delta), [T1,T2,node(not_overruled(Assumption),Tree_list)]))
:-
    member(modal(Op1,A,B), Gamma),
    member_norm(Assumption,Op2,C,D,D_ass_list),
    conflicts(asmp(Facts,D_ass_list,Op_char,Rel),Op1,Op2),
    % the assumption is applicable:
    prove(classic,asmp(Facts,D_ass_list,Op_char,Rel), seq([B],[D]), T1),
    % the condition is in conflict with the assumption:
    confl(asmp(Facts,D_ass_list,Op_char,Rel), Op2,Op1, C,A, Seq1),
    prove(classic,asmp(Facts,D_ass_list,Op_char,Rel), Seq1, T2),
    % filter out the assumptions for conflicting operators:
    include(applicable_assumption(asmp(Facts,D_ass_list,Op_char,Rel),
				  modal(Op2,C,D)), D_ass_list,
	    Outer_list),
    not_overruled(l,asmp(Facts,D_ass_list,Op_char,Rel),
			  modal(Op1,A,B), Assumption, Outer_list,
	    Tree_list),!. % cut for efficiency
/* modern version (for the new way of specificity handling) */
prove(modern,asmp(Facts,D_ass_list,Op_char,Rel), seq(Gamma,Delta),
      node(asmpL(Op1,Assumption), seq([modal(Op1,A,B)],[]),
	   seq(Gamma,Delta), [T1,T2,node(not_overruled(Assumption),Tree_list)]))
:-
    member(modal(Op1,A,B), Gamma),
    member_norm(Assumption,Op2,C,D,D_ass_list),
    conflicts(asmp(Facts,D_ass_list,Op_char,Rel),Op1,Op2),
    % the assumption is applicable:
    prove(modern,asmp(Facts,D_ass_list,Op_char,Rel), seq([B],[D]), T1),
    % the condition is in conflict with the assumption:
    confl(asmp(Facts,D_ass_list,Op_char,Rel), Op2,Op1, C,A, Seq1),
    prove(modern,asmp(Facts,D_ass_list,Op_char,Rel), Seq1, T2),
    % filter out the assumptions for conflicting operators:
    % Retrieve the cached conflict list for Op2(C,D):
    conflicting_assumptions(modal(Op2,C,D),Outer_list),
    not_overruled_modern(l,asmp(Facts,D_ass_list,Op_char,Rel),
			  modal(Op1,A,B), Assumption, Outer_list,
	    Tree_list),!. % cut for efficiency
	    

/* not_overruled
   The assumption modal(Op2,C,D) is not overruled by a more specific
   or conflicting undefeated assumption modal(Op3,E,F) from a list of
   possibly conflicting assumptions.
   Takes the side of the assumption rule as the first argument.
*/
/* classic version (for specificity handling as in the DEON 2020 paper) */
not_overruled(_,_,_,_,[],[]).
% clause for not applicable (condition of the possibly conflicting
% modal(Op3,E,F) is not implied by B)
not_overruled(Side,Asmp, modal(Op1,A,B), Assumption
	      , [Fml3|Tail_ass], [node(notapplicable(Fml3,seq([B],[F])
						    ))|Tail_tree]) :-
    modal_arguments(Fml3,_,_,F),
    \+ prove(classic,Asmp, seq([B],[F]), _),
    not_overruled(Side,Asmp, modal(Op1,A,B), Assumption, Tail_ass, Tail_tree).
% clause for no conflict (no conflict between the possibly conflicting
% modal(Op3,E,F) and A
not_overruled(r,Asmp, modal(Op1,A,B), Assumption
	      , [Fml3|Tail_ass], [node(noconflict(Fml3,Seq))|
	      Tail_tree]) :-
    modal_arguments(Fml3,Op3,E,_),
    confl(Asmp,Op3,Op1,E,A,Seq),
    \+ prove(classic,Asmp, Seq, _),
    not_overruled(r,Asmp, modal(Op1,A,B), Assumption,Tail_ass, Tail_tree).
not_overruled(l,Asmp, modal(Op1,A,B), Assumption
	      , [Fml3|Tail_ass], [node(notimplied(Fml3,Seq))|
	      Tail_tree]) :-
    modal_arguments(Fml3,Op3,E,_),
    impl(Asmp,Op3,Op1,E,A,Seq),
    \+ prove(classic,Asmp, Seq, _),
    not_overruled(l,Asmp, modal(Op1,A,B), Assumption,Tail_ass, Tail_tree).
% clause for superiority: the assumption modal(Op3,C,D) is superior to
% modal(Op3,C,D)
not_overruled(Side, asmp(Facts,D_ass,Ops,Sup_rel), modal(Op1,A,B)
	      , (Norm1:Assumption1), [(Norm3:Fml3)|Tail_ass]
	      , [node(superior(Norm1:Assumption1, Norm3:Fml3))|
		 Tail_tree]) :-
    member(Norm1 beats Norm3, Sup_rel),
    not_overruled(Side, asmp(Facts,D_ass,Ops,Sup_rel), modal(Op1,A,B)
		  , (Norm1:Assumption1), Tail_ass, Tail_tree).
% clause for not more specific than the original Assumption = modal(Op2,C,D) and
% also there is another more specific one which overrules the possibly
% conflicting modal(Op3,E,F) again.
not_overruled(Side,asmp(Facts,D_ass,Op_char,Rel), modal(Op1,A,B), Assumption
	      , [Fml3|Tail_ass]
	      , [node(notoverruled(Fml3,seq([F],[D]),[T]))| Tail_tree]) :-
    % modal(Op3,E,F) is not more specific than modal(Op2,C,D):
    modal_arguments(Assumption,_,_,D),
    modal_arguments(Fml3,Op3,E,F),
    \+ prove(classic,asmp(Facts,D_ass,Op_char,Rel), seq([F],[D]),_),
    % modal(Op3,E,F) is overridden by another more specific assumption:
    include(applicable_assumption(asmp(Facts,D_ass,Op_char,Rel),
				  modal(Op3,E,F)), D_ass, Inner_list),
    overridden(Side,asmp(Facts,D_ass,Op_char,Rel), modal(Op1,A,B),
	       Assumption, Fml3, Inner_list, T),
    not_overruled(Side,asmp(Facts,D_ass,Op_char,Rel), modal(Op1,A,B),
		  Assumption,Tail_ass, Tail_tree).


/* not_overruled_modern
   for specificity handling the modern way using conflict lists

   The assumption modal(Op2,C,D) is not overruled by a more specific
   or conflicting undefeated assumption modal(Op3,E,F) from a list of
   possibly conflicting assumptions.
   Takes the side of the assumption rule as the first argument.

   The additional formula modal(Op1,A,B) is the one we want to
   derive from the assumption modal(Op2,C,D) resp. Assumption.

   NOTE: We only use this on an assumption and its conflict list,
   hence we already know that every assumption in the list of possibly
   conflicting assumptions is indeed conflicting and don't need to
   check it again.
*/
not_overruled_modern(_,_,_,_,[],[]).

% clause for not applicable (condition of the possibly conflicting
% modal(Op3,E,F) is not implied by B)
not_overruled_modern(Side,Asmp, modal(Op1,A,B), Assumption
	      , [Fml3|Tail_ass], [node(notapplicable(Fml3,seq([B],[F])
						    ))|Tail_tree]) :-
    modal_arguments(Fml3,_,_,F),
    \+ prove(modern,Asmp, seq([B],[F]), _),
    not_overruled_modern(Side,Asmp, modal(Op1,A,B), Assumption, Tail_ass, Tail_tree).

/* The clause for no conflict (no conflict between the possibly
 * conflicting modal(Op3,E,F) and A) is obsolete for the "modern"
 * reasoning, since we only look at the conflict list anyways, and
 * also because we consider conflicts with Assumption and not with
 * modal(Op1,A,B).
*/

% clause for superiority: the assumption modal(Op2,C,D) is superior to
% modal(Op3,E,F)
not_overruled_modern(Side, asmp(Facts,D_ass,Ops,Sup_rel), modal(Op1,A,B)
	      , (Norm1:Assumption1), [(Norm3:Fml3)|Tail_ass]
	      , [node(superior(Norm1:Assumption1, Norm3:Fml3))|
		 Tail_tree]) :-
    member(Norm1 beats Norm3, Sup_rel),
    not_overruled_modern(Side, asmp(Facts,D_ass,Ops,Sup_rel), modal(Op1,A,B)
		  , (Norm1:Assumption1), Tail_ass, Tail_tree).

% clause for not more specific than the original Assumption = modal(Op2,C,D) and
% also there is another more specific one which overrules the possibly
% conflicting modal(Op3,E,F) again.
not_overruled_modern(Side,asmp(Facts,D_ass,Op_char,Rel), modal(Op1,A,B), Assumption
	      , [Fml3|Tail_ass]
	      , [node(notoverruled(Fml3,seq([F],[D]),[T]))| Tail_tree]) :-
    % Fml3 = modal(Op3,E,F) is not more specific than Assumption = modal(Op2,C,D):
    modal_arguments(Assumption,_,_,D),
    modal_arguments(Fml3,Op3,E,F),
    % NOTE: Scope for efficiency improvement here: pre-calculate the
    % "more specific" part of the conflict list for each assumption.
    \+ prove(modern,asmp(Facts,D_ass,Op_char,Rel), seq([F],[D]),_),
    % modal(Op3,E,F) = Fml3 is overridden by another more specific assumption:
    conflicting_assumptions(modal(Op3,E,F),Confl_list_Fml3),
    overridden_modern(Side,asmp(Facts,D_ass,Op_char,Rel), modal(Op1,A,B),
	       Assumption, Fml3, Confl_list_Fml3, T),
    not_overruled_modern(Side,asmp(Facts,D_ass,Op_char,Rel), modal(Op1,A,B),
		  Assumption,Tail_ass, Tail_tree).


/* overridden
   true if the formula modal(Op3,E,F) is overridden by a formula from
   the list of relevant deontic assumptions.
   Takes the side of the assumption rule as the first argument
*/
/* classic version (for specificity handling as in the DEON 2020 paper) */
overridden(r,Asmp,  modal(Op1,A,B), _, Fml3
	   , [Fml4|_],
	   node(overrides(Fml4, Fml3),[T1,T2,T3]))
:-
    modal_arguments(Fml3,Op3,E,F),
    modal_arguments(Fml4,Op4,X,Y),
    nbeats(Asmp,modal(Op3,E,F),modal(Op4,X,Y)),
    prove(classic,Asmp,seq([B],[Y]),T1),
    prove(classic,Asmp,seq([Y],[F]),T2),
    impl(Asmp,Op4,Op1,X,A,Seq),
    prove(classic,Asmp,Seq,T3).
overridden(l,Asmp,  modal(Op1,A,B), _, Fml3
	   , [Fml4|_],
	   node(overrides(Fml4, Fml3),[T1,T2,T3]))
:-
    modal_arguments(Fml3,Op3,E,F),
    modal_arguments(Fml4,Op4,X,Y),
    nbeats(Asmp,modal(Op3,E,F),modal(Op4,X,Y)),
    prove(classic,Asmp,seq([B],[Y]),T1),
    prove(classic,Asmp,seq([Y],[F]),T2),
    confl(Asmp,Op4,Op1,X,A,Seq),
    prove(classic,Asmp,Seq,T3).
overridden(Side,Asmp, F1, F2, F3, [_|Tail_list], Tree) :-
    overridden(Side,Asmp, F1,F2,F3, Tail_list, Tree).


/* overridden_modern
   true if the formula modal(Op3,E,F) = Fml3 is overridden by a
   formula Fml4 = modal(Op4,X,Y) from the list of relevant deontic
   assumptions. 
   Takes the side of the assumption rule as the first argument
*/
/* modern version (for alternative way of specificity handling) */
/* NOTE_TMP: We only call this with the conflict_list of Fml3, so we
 * know that sequent witnessing the conflict between Fml3 and Fml4 is
 * provable already. This shouldn't be too bad, though, because the
 * proof should be reasonably quick to find.
*/
overridden_modern(_,Asmp,  modal(_,_,B), _, Fml3
	   , [Fml4|_],
	   node(overrides(Fml4, Fml3),[T1,T2,T3]))
:-
    modal_arguments(Fml3,Op3,E,F),
    modal_arguments(Fml4,Op4,X,Y),
    nbeats(Asmp,modal(Op3,E,F),modal(Op4,X,Y)),
    prove(modern,Asmp,seq([B],[Y]),T1),
    prove(modern,Asmp,seq([Y],[F]),T2),
    confl(Asmp,Op3,Op4,E,X,Seq),
    prove(modern,Asmp,Seq,T3).
overridden_modern(Side,Asmp, F1, F2, F3, [_|Tail_list], Tree) :-
    overridden_modern(Side,Asmp, F1,F2,F3, Tail_list, Tree).


/* applicable_assumption
   true if given the assumptions in the first argument, the
   particular deontic assumption in the second argument might be
   overruled by the deontic assumption in the last argument.
*/
applicable_assumption(Assumptions,modal(Op,_,_), Fml) :-
    modal_arguments(Fml,Op2,_,_),
    conflicts(Assumptions,Op2,Op).


/* no_conflict_p
   true if there is no conflict wrt the P axiom
*/
/* classic version (for specificity handling as in the DEON 2020 paper) */
no_conflict_p(Asmp,Op,A,node(no_p_conflict(Op,Seq))) :-
    nontrivial(Asmp,Op),
    confl(Asmp,Op,Op,A,A,Seq),
    \+ prove(classic,Asmp,Seq,_).
no_conflict_p(Asmp,Op,_,node(no_p_conflict(na))) :-
    \+ nontrivial(Asmp,Op).
		  

/* no_conflict_p_modern
   true if there is no conflict wrt the P axiom
*/
/* modern version with alternative specificity handling */
no_conflict_p_modern(Asmp,Op,A,node(no_p_conflict(Op,Seq))) :-
    nontrivial(Asmp,Op),
    confl(Asmp,Op,Op,A,A,Seq),
    \+ prove(modern,Asmp,Seq,_).
no_conflict_p_modern(Asmp,Op,_,node(no_p_conflict(na))) :-
    \+ nontrivial(Asmp,Op).
		  

/* type /3
   true if Op is of type Type according to the assumptions
*/
type(asmp(_,_,ops(Op_list,_,_,_),_),Op,Type) :-
    member((Op,Type), Op_list).


/* impl predicate
   implements the generic inclusion sequent, depending on whether Op1
   and Op2 are of prohibition or obligation type.
*/
impl(Assumptions,Op1,Op2,F1,F2,seq([F1],[F2])) :-
    type(Assumptions,Op1,obl),
    type(Assumptions,Op2,obl).
impl(Assumptions,Op1,Op2,F1,F2,seq([F1,F2],[])) :-
    type(Assumptions,Op1,obl),
    type(Assumptions,Op2,for).
impl(Assumptions,Op1,Op2,F1,F2,seq([],[F1,F2])) :-
    type(Assumptions,Op1,for),
    type(Assumptions,Op2,obl).
impl(Assumptions,Op1,Op2,F1,F2,seq([F2],[F1])) :-
    type(Assumptions,Op1,for),
    type(Assumptions,Op2,for).


/* implies /3:
   true if Op1 implies Op2 according to the assumptions
*/
implies(_,Op,Op).
implies(asmp(_,_,ops(_,Inclusion_list,_,_),_),Op1,Op2) :-
    member(Op1 -> Op2, Inclusion_list).


/* confl predicate
   implements the generic conflict sequent, depending on the types of
   Op1 and Op2.
*/
confl(Assumptions,Op1,Op2,F1,F2,seq([F1,F2],[])) :-
    type(Assumptions, Op1, obl),
    type(Assumptions, Op2, obl).
confl(Assumptions,Op1,Op2,F1,F2,seq([F1],[F2])) :-
    type(Assumptions, Op1, obl),
    type(Assumptions, Op2, for).
confl(Assumptions,Op1,Op2,F1,F2,seq([F2],[F1])) :-
    type(Assumptions, Op1, for),
    type(Assumptions, Op2, obl).
confl(Assumptions,Op1,Op2,F1,F2,seq([],[F1,F2])) :-
    type(Assumptions, Op1, for),
    type(Assumptions, Op2, for).


/* conflicts /3:
   true if Op1 conflicts with Op2 according to the assumptions
*/    
conflicts(asmp(_,_,ops(_,_,Conflict_list,_),_),Op1,Op2) :-
    member(confl(Op1,Op2),Conflict_list).


/* nontrivial /2:
   true if Op is nontrivial (i.e., has the P axiom) according to the
   assumptions.
*/
nontrivial(asmp(_,_,ops(_,_,_,P_list),_),Op) :-
    member(nt(Op),P_list).


/* nbeats /3:
   true if the formula F1 is not superior to the formula F2 according
   to Assumptions
*/
nbeats(Assumptions,F1,F2) :-
    \+ beats(Assumptions,F1,F2).


/* beats /3:
   true if the formula F1 beats the formula F2 according to
   Assumptions
*/
/* NOTE: added here that the textuelle Bestimmungen (i.e., norms of
 * the form bb(X):F or b(X):F) ALWAYS beat the bauordnung (i.e., norms
 * of the form bo(X):F)! Change the last two clauses is if this is not
 * desired!
*/
beats(asmp(_,D_assumptions,_,Sup_rel),F1,F2) :-
    member((Norm1:F1),D_assumptions),
    member((Norm2:F2),D_assumptions),
    member(Norm1 beats Norm2, Sup_rel).
beats(asmp(_,D_assumptions,_,_),F1,F2) :-
    member((b(_):F1),D_assumptions),
    member((bo(_):F2),D_assumptions).
beats(asmp(_,D_assumptions,_,_),F1,F2) :-
    member((bb(_):F1),D_assumptions),
    member((bo(_):F2),D_assumptions).


/* merge_sequent
 * merge two sequents into a third.
*/
merge_sequent(seq(A,B), seq(C,D), seq(E,F)) :-
    append(A,C,E),
    append(B,D,F).


/* member_norm /5
   true if a formula modal(Op,A,B) or Norm:modal(Op,A,B) is in the
   list of deontic assumptions and its arguments are Op, A, B
*/
member_norm(modal(Op,A,B),Op,A,B,D_ass) :-
    member(modal(Op,A,B), D_ass).
member_norm((Norm:modal(Op,A,B)),Op,A,B,D_ass) :-
    member((Norm:modal(Op,A,B)), D_ass).


/* modal_arguments
   true if the norm in the first argument has the operator in the
   second and the arguments in the last two.
*/
modal_arguments(modal(Op,A,B),Op,A,B).
modal_arguments((_:modal(Op,A,B)),Op,A,B).


