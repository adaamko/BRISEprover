/*
Copyright 2020 Bjoern Lellmann

    This file is part of deonticProver 2.1.

    deonticProver 2.1 is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    deonticProver 2.1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with deonticProver 2.1.  If not, see <http://www.gnu.org/licenses/>.
*/

/* preprocessing.pl
 * contains the predicates used for preprocessing the input
*/


/* preprocess /2
   true if second argument is the preprocessed version of first
   argument.
   add at(.) around atoms everywhere; converts facts to sequents and
   saturates under cuts; verifies that the operator types are fine;
   saturates the operators under inclusions, conflicts, etc; adds
   nt(.) around operators for nontriviality assumptions; checks
   that the superiority relation is acyclic.
*/
preprocess(classic,asmp(Facts,D_Ass, ops(Ops, Op_Incl, Op_Confl, Op_P),
		Sup_Rel), asmp(Facts_new, D_Ass_new,
			       ops(Ops, Op_Incl1, Op_Confl_new,
				   Op_P_new), Sup_Rel),[]) :-
    maplist(modalised,Facts,Facts1),
    maplist(added_at,Facts1,Facts2),
    saturated_facts(Facts2,Facts_new),
    maplist(modalised,D_Ass,D_Ass1),
    maplist(added_at,D_Ass1,D_Ass_new),
    maplist(add_nt,Op_P, Op_P1),
    verified_op_types(Ops,_),
    verified_assumptions(D_Ass1,Ops,_),
    saturated_inclusions(Op_Incl,Op_Incl1),
    saturated_conflicts(Op_Incl1, Op_Confl, Op_Confl_new),
    saturated_P(Op_Incl1, Op_Confl_new, Op_P1, Op_P_new),
    verified_superiority_relation(Sup_Rel),
    acyclic(D_Ass_new,Sup_Rel).
% Modern version using conflict lists:
preprocess(modern,asmp(Facts,D_Ass, ops(Ops, Op_Incl, Op_Confl, Op_P),
		Sup_Rel), asmp(Facts_new, D_Ass_new,
			       ops(Ops, Op_Incl1, Op_Confl_new,
				   Op_P_new), Sup_Rel),[]) :-
    maplist(modalised,Facts,Facts1),
    maplist(added_at,Facts1,Facts2),
    saturated_facts(Facts2,Facts_new),
    maplist(modalised,D_Ass,D_Ass1),
    maplist(added_at,D_Ass1,D_Ass_new),
    maplist(add_nt,Op_P, Op_P1),
    verified_op_types(Ops,_),
    verified_assumptions(D_Ass1,Ops,_),
    saturated_inclusions(Op_Incl,Op_Incl1),
    saturated_conflicts(Op_Incl1, Op_Confl, Op_Confl_new),
    saturated_P(Op_Incl1, Op_Confl_new, Op_P1, Op_P_new),
    verified_superiority_relation(Sup_Rel),
    acyclic(D_Ass_new,Sup_Rel),
    retractall(conflicting_assumptions(_,_)),
    make_conflict_lists(asmp(Facts_new, D_Ass_new,
			       ops(Ops, Op_Incl1, Op_Confl_new,
				   Op_P_new), Sup_Rel), D_Ass_new).
% Old version where the permissions are automatically added:
/*
preprocess(asmp(Facts,D_Ass, ops(Ops, Op_Incl, Op_Confl, Op_P),
		Sup_Rel), asmp(Facts_new, D_Ass_new,
			       ops(Ops_new, Op_Incl_new, Op_Confl_new,
				   Op_P_new), Sup_Rel)) :-
    maplist(modalised,Facts,Facts1),
    maplist(added_at,Facts1,Facts2),
    saturated_facts(Facts2,Facts_new),
    maplist(modalised,D_Ass,D_Ass1),
    maplist(added_at,D_Ass1,D_Ass_new),
    verified_op_types(Ops),
    saturated_inclusions(Op_Incl,Op_Incl1),
    add_permissions(ops(Ops,Op_Incl1, Op_Confl, Op_P),
		    ops(Ops_new, Op_Incl_new, Op_Confl1, Op_P1)),
    saturated_conflicts(Op_Incl_new, Op_Confl1, Op_Confl_new),
    saturated_P(Op_Incl_new, Op_Confl_new, Op_P1, Op_P_new),
    acyclic(D_Ass_new,Sup_Rel).
*/

/* add_permissions /2
   true if adding the permission operators for every operator in Ops
   together with conflict only with the corresponding operator yields
   Ops_new.
*/
add_permissions(ops([],Incl,Confl,Rec), ops([],Incl,Confl,Rec)).
add_permissions(ops([(Op1,Type)|Ops],Incl,Confl,Rec),
		ops([(Op1,Type),(per(Op1),obl)|Ops_new], Incl_new
		    , [confl(Op1,per(Op1))|Confl_new], Rec_new)) :-
    add_permissions(ops(Ops,Incl,Confl,Rec),
		    ops(Ops_new, Incl_new, Confl_new, Rec_new)).
		    
					     

/* added_at
 * true if second argument is first argument with at(AT) instead of
 * atom AT
*/
added_at(false,false).
added_at(true,true).
added_at(neg(A),neg(B)) :- added_at(A,B).
added_at(and(A,B),and(C,D)) :- added_at(A,C), added_at(B,D).
added_at(or(A,B),or(C,D)) :- added_at(A,C), added_at(B,D).
added_at(->(A,B), ->(C,D)) :- added_at(A,C), added_at(B,D).
added_at(modal(Op,A,B),modal(Op,C,D)) :- added_at(A,C), added_at(B,D).
added_at(Norm:modal(Op,A,B),Norm:modal(Op,C,D)) :- added_at(A,C), added_at(B,D).
added_at(A,at(A)) :- atom(A).
added_at(A,at(A)) :-
    A =.. [Var|_],
    variable_with_arguments(Var).
added_at(A beats B, C beats D) :- added_at(A,C), added_at(B,D).
added_at(seq(L,N), seq(Lat,Nat)) :-
    maplist(added_at,L,Lat),
    maplist(added_at,N,Nat).
added_at(Complex,at(Complex)) :-
    Complex =.. [Op|_],
    variable_with_arguments(Op).

/* modalised /2
   true if all the non-propositional operators in the first argument
   are replaced with modal(Op) in the second argument
*/
modalised(A,A) :- atom(A).
modalised(at(A),at(A)).
modalised(neg(A),neg(B)) :- modalised(A,B).
modalised(Norm:A, Norm:B) :- modalised(A,B).
modalised(Complex,Complex) :-
    Complex =.. [Op|_],
    variable_with_arguments(Op).
modalised(Complex,Complex1) :-
    Complex =.. [Op|Args],
    member(Op, [neg,and,or,->]),
    maplist(modalised,Args,Args1),
    Complex1 =.. [Op|Args1].
/*
modalised(Complex,Complex1) :-
    Complex =.. [per,Op|Args],
    maplist(modalised,Args,Args1),
    Complex1 =.. [modal,per(Op)|Args1].
*/
modalised(Complex,Complex1) :-
    Complex =.. [Op|Args],
    \+ member(Op, [at,neg,:,and,or,->]),
    maplist(modalised,Args,Args1),
    Complex1 =.. [modal,Op|Args1].
		  

/* fact_and_sequent /2
 * for converting clauses of atoms or negated atoms to sequents.
*/
fact_and_sequent(Left -> Right, seq(List1,List2)) :-
    fact_and_sequent_left(Left, seq(List_left1,List_left2)),
    fact_and_sequent_right(Right, seq(List_right1, List_right2)),
    merge_sequent(seq(List_left1, List_left2),
		  seq(List_right1, List_right2), seq(List1, List2)).
% clauses for the left hand side
fact_and_sequent_left(A and B, seq(List1,List2)) :-
    fact_and_sequent_left(A,seq(List1_left,List1_right)),
    fact_and_sequent_left(B,seq(List2_left,List2_right)),
    merge_sequent(seq(List1_left, List1_right),
		  seq(List2_left, List2_right), seq(List1, List2)).
fact_and_sequent_left(neg A, seq([],[at(A)])) :-
    atom(A).
fact_and_sequent_left(at(A),seq([at(A)],[])).
fact_and_sequent_left(true,seq([],[])).
fact_and_sequent_left(false,seq([false],[])).
fact_and_sequent_left(A, seq([at(A)],[])) :-
    atom(A).
% clauses for the right hand side
fact_and_sequent_right(A or B, seq(List1,List2)) :-
    fact_and_sequent_right(A,seq(List1_left,List1_right)),
    fact_and_sequent_right(B,seq(List2_left,List2_right)),
    merge_sequent(seq(List1_left, List1_right),
		  seq(List2_left, List2_right), seq(List1, List2)).
fact_and_sequent_right(neg A, seq([at(A)],[])) :-
    atom(A).
fact_and_sequent_right(at(A), seq([],[at(A)])).
fact_and_sequent_right(true, seq([],[true])).
fact_and_sequent_right(false, seq([],[])).
fact_and_sequent_right(A, seq([],[at(A)])) :-
    atom(A).


/* saturated_facts /2
   true if first argument is a list of facts given as formulae, and
   second argument is a list of sequents obtained from the formulae by
   translating to sequents, then saturating under cuts
*/
saturated_facts(Facts,Facts_saturated) :-
    maplist(fact_and_sequent,Facts,Facts_seqs),
    saturate(Facts_seqs,Facts_saturated).


/* saturate /2: 
 * true if saturating Start under cuts yields Goal (disregarding
 * sequents which can be derived from contained sequents using
 * weakening, and initial sequents)
*/
saturate(Start,Goal) :-
    member(seq(Gamma,Delta),Start),
    list_to_set(Gamma,Sigma),
    list_to_set(Delta,Pi),
    \+ member(seq(Sigma,Pi),Start),!,
    saturate([seq(Sigma,Pi)|Start],Goal).
saturate(Start,Goal) :-
    member(seq(Sigma,Pi),Start),
    select(A,Pi,Pi1),
    member(seq(Omega,Theta),Start),
    select(A,Omega,Omega1),
    append(Sigma,Omega1,Gamma),
    append(Pi1,Theta,Delta),
    list_to_set(Gamma,Gamma_Set),
    list_to_set(Delta,Delta_Set),
    intersection(Gamma_Set,Delta_Set,[]),
    \+ subsumed(seq(Gamma_Set,Delta_Set),Start),!,
    saturate([seq(Gamma,Delta)|Start],Goal).
saturate(Goal,Goal).


/* subsumed /2
 * true if the sequent in the first argument can be obtained from a
 * sequent in the list in the second argument using weakening
*/
subsumed(seq(Gamma_Set,Delta_Set),[seq(Sigma,Pi)|Tail]) :-
    subset(Sigma,Gamma_Set),
    subset(Pi,Delta_Set)
    ;
    subsumed(seq(Gamma_Set,Delta_Set),Tail).



/* verified_op_types
   TODO [x]
   - check that every operator has only one type
   - check that every formula in the assumptions has an operator with
   a type
   - check that all operators in the inclusion, conflict,
   nontriviality statements have a type
*/

/* verified_op_types /2
   true if Bad_ops contains those operators which are mentioned at
   least twice in Ops, possibly with repetitions
*/
verified_op_types([],[]).
verified_op_types([(Op,_)|Tail_ops],[Op|Tail_bad_ops]) :-
    member((Op,_),Tail_ops),
    verified_op_types(Tail_ops,Tail_bad_ops).
verified_op_types([(Op,_)|Tail_ops],Bad_ops) :-
    \+ member((Op,_),Tail_ops),
    verified_op_types(Tail_ops,Bad_ops).


/* verified_assumptions /3
   true if Bad_Ass contains those assumptions from D_Ass which do not
   have an operator with type in Ops
*/
verified_assumptions([],_,[]).
verified_assumptions([Ass|D_Ass],Ops,Bad_Ass) :-
    modal_arguments(Ass,Op,_,_),
    member((Op,_),Ops),
    verified_assumptions(D_Ass,Ops,Bad_Ass).
verified_assumptions([Ass|D_Ass],Ops,[Op|Bad_Ass]) :-
    modal_arguments(Ass,Op,_,_),
    \+ member((Op,_),Ops),
    verified_assumptions(D_Ass,Ops,Bad_Ass).



/* saturated_inclusions
   true if saturating Incl under transitivity yields Incl_sat
*/
saturated_inclusions(Incl,Incl_sat) :-
    member(Op1 -> Op2, Incl),
    member(Op2 -> Op3, Incl),
    \+ member(Op1 -> Op3, Incl),
    saturated_inclusions([Op1 -> Op3|Incl],Incl_sat).
saturated_inclusions(Incl,Incl).


/* saturated_conflicts
   true if saturating Confl under symmetry and the inclusions in Incl
   yields Confl_sat 
*/
saturated_conflicts(Incl, Confl, Confl_sat) :-
    member(confl(Op1,Op2), Confl),
    \+ member(confl(Op2,Op1), Confl),
    saturated_conflicts(Incl, [confl(Op2,Op1)|Confl], Confl_sat).
saturated_conflicts(Incl, Confl, Confl_sat) :-
    member(confl(Op1,Op2), Confl),
    member(Op3 -> Op1, Incl),
    \+ member(confl(Op3, Op2), Confl),
    saturated_conflicts(Incl,[confl(Op3,Op2)|Confl], Confl_sat).
saturated_conflicts(_,Confl,Confl).


/* add_nt
   true for an operator Op and its nontriviality statement nt(Op)
*/
add_nt(X,nt(X)).


/* saturated_P
   true if saturating Op_P under preimages of Op_incl yields Op_P_new
   TODO [x]
*/
saturated_P(Op_incl, Op_confl, Op_P, Op_P_new) :-
    member(nt(Op),Op_P),
    member(Op2 -> Op, Op_incl),
    \+ member(nt(Op2), Op_P),
    saturated_P(Op_incl, Op_confl, [nt(Op2)|Op_P], Op_P_new).
saturated_P(_,_,Op_P,Op_P).


/* NOTE: CHANGED THIS HERE
   MAKE SURE EVERYTHING STILL WORKS (NOT YET IN THE ONLINE SYSTEM!)
   TODO []check this!
*/
/* verified_superiority_relation /1
   true if the superiority relation is a list of formulae beats(Ass_1,Ass_2).
*/
verified_superiority_relation([]).
verified_superiority_relation([beats(_,_)|Tail]) :-
    verified_superiority_relation(Tail).


/* acyclic /2
 * True if a list of srauta and a relation (given as a list) does not
 * contain any cyles, i.e., any srauta (_:Fml1), (_:Fml2) with Fml1
 * beats Fml2 and Fml2 beats Fml1
*/
acyclic(_,[]).
acyclic(D_Ass,[Relation|Tail]) :-
    \+ cyclic_relation(D_Ass,[Relation|Tail]).


/* cyclic_relation /2
 * true if there are two norms in D_Ass giving a cycle wrt. Relation
*/
cyclic_relation(D_Ass,Relation) :-
    member((_:Fml1), D_Ass),
    member((_:Fml2), D_Ass),
    beats(asmp([],D_Ass,[],Relation), Fml1, Fml2),
    beats(asmp([],D_Ass,[],Relation), Fml2, Fml1).


/* cycles_in_relation /3
 * true if Cycle_list contains all tuples of normed deontic
 * assumptions creating a cycle.
*/
/*
cycles_in_relation(Relation,Cycle_list) :-
    findall(cycle(Norm1,Norm2), (member(beats(Norm1, Norm2), Relation),
				 member(beats(Norm2, Norm1), Relation)
				), List1),
    remove_symmetries(List1,[],Cycle_list).
*/
/* remove_symmetries /3
 * removes duplicates and symmetric entries
*/
/*remove_symmetries([],Cycles,Cycles).
remove_symmetries([cycle(Norm1,Norm2)|Tail],Reduced_list,Tail2) :-
    (member(cycle(Norm1,Norm2),Reduced_list)
    ;
    member(cycle(Norm2,Norm1),Reduced_list)),
    remove_symmetries(Tail,Reduced_list,Tail2).
remove_symmetries([cycle(Norm1,Norm2)|Tail]
		  , Reduced_tail, Tail2) :-
    \+ member(cycle(Norm1,Norm2), Reduced_tail),
    \+ member(cycle(Norm2,Norm1), Reduced_tail),
    remove_symmetries(Tail,[cycle(Norm1,Norm2)|Reduced_tail], Tail2).
*/				 


/* added_facts
 * DCG for adding facts for a list of examples.
 * facts_plangebiet//1 is specified in assumptionhandler.pl
*/
/*
added_facts(Facts,[]) --> Facts.
added_facts(Facts,[Ex|Tail]) --> {facts(Ex,List)}, List, added(Facts,Tail).
*/
% new version:
added_facts(Facts,List) --> Facts, lift_DCG(facts_plangebiet,List).


/* added_assumptions
 * DCG for adding deontic assumptions for a list of examples
 * obligations_plangebiet//1 is specified in assumptionhandler.pl
*/
/*added_assumptions(D_ass,[]) --> D_ass.
added_assumptions(D_ass,[Ex|Tail])
--> {obligations_plangebiet(Ex,List)},  List,
    added_assumptions(D_ass,Tail).
*/
% new version:
added_assumptions(D_ass, List)
--> D_ass, lift_DCG(obligations_plangebiet,List).


/* NEED:
   Given a list of examples / plangebiete:
   - create list of factual assumptions from these => facts_plangebiet_list
   - create list of deontic assumptions from these => obligations_plangebiet_list
   append them to Facts and D_assumptions from the input
   Further: 
   - add background facts to the factual assumptions
   - add obligations from Bauordnung to deontic_assumptions
*/


/* lift_DCG//2
 * lift a DCG body defined on single objects to a list of objects and
 * concatenate the outputs.
*/
lift_DCG(_,[]) --> [].
lift_DCG(Body,[A|Tail]) -->
    {T =.. [Body,A], phrase(T,L)}, L, lift_DCG(Body,Tail).
		

/* confl_list /3
 * given a deontic formula, filter out the conflict list for that
 * formula, i.e., the list of deontic assumptions for which the
 * content is in conflict and which are not inferior.
*/
% NOTE: possibly scope for more efficency: assume that Ass1 is of the
% form modal(Op1,A,B) from the beginning.
% NOTE2: possibly scope for making things nicer: store the derivation
% of Confl(A,C) together with the formula Ass2.
confl_list(Ass1, Assumptions, [Ass2|Tail_ass], [Ass2|Tail_list]) :-
    modal_arguments(Ass1,Op1,A,B),
    modal_arguments(Ass2,Op2,C,D),
    conflicts(Assumptions,Op1,Op2),
    confl(Assumptions,Op1,Op2,A,C,Seq),
    prove(modern,Assumptions,Seq,_),
    nbeats(Assumptions,modal(Op1,A,B), modal(Op2,C,D)),
    confl_list(Ass1, Assumptions, Tail_ass, Tail_list).
confl_list(Ass, Assumptions, [_|Tail_ass], Tail_list) :-
    confl_list(Ass, Assumptions, Tail_ass, Tail_list).
confl_list(_, _, [], []).					


/* confl_list_test
   for testing
*/
confl_list_test(Ass, AsmpList,L) :-
    confl_list(Ass,asmp([],[],ops([(obl,obl),(per,obl),(for,for)], [], [confl(obl,obl), confl(obl,per), confl(per,obl), confl(obl,for), confl(for,for), confl(per,for)],[]),[]), AsmpList,L).


/* make_conflict_lists /2
   constructs the conflict list for every deontic assumption and adds
   them to the program
   ATTENTION: This uses assert!
*/
make_conflict_lists(_,[]).
make_conflict_lists(asmp(Facts,D_Ass,Ops,Sup),[Ass|TailAss]) :-
    confl_list(Ass,asmp(Facts,D_Ass,Ops,Sup),D_Ass,Confl_list),
    assertz(conflicting_assumptions(Ass,Confl_list)),
    make_conflict_lists(asmp(Facts,D_Ass,Ops,Sup),TailAss).

mcl_test(List) :-
    make_conflict_lists(asmp([],List,ops([(obl,obl),(per,obl),(for,for)], [], [confl(obl,obl), confl(obl,per), confl(per,obl), confl(for,obl), confl(obl,for), confl(for,for), confl(per,for)],[]),[]),List).

/* CONTINUE HERE:
   [x] add make_conflict_lists to preprocessing
   [ ] add the call to conflict lists to the assumption rules
   [ ] retract all conflicting_assumptions before everything
   [ ] retract all conflicting_assumptions after everything
*/
