/*
Copyright 2020 Bjoern Lellmann

    This file is part of BRISEprover.

    BRISEprover is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    deonticProver 2.1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BRISEprover.  If not, see <http://www.gnu.org/licenses/>.
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

/* List of additional variables to be used with arguments
*/
variable_with_arguments(Op) :-
    member(Op,[height, width, max_height, max_width, min_height,
	       min_width, measure, max_measure, min_measure, area,
	       plangebiet, bauland, grundflaeche, widmung, bauklasse,
	       bauweise, bb, baulinie, baufluchtlinie, grenzlinie]).

/* load parts for prettyprinting and for preprocessing
*/
:- ensure_loaded([prettyprinting]).
:- ensure_loaded([preprocessing]).
/* load example formalisation
*/
:- ensure_loaded([assumptionhandler]).
:- ensure_loaded([pd7602]).
%:- ensure_loaded([pd7601]).

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

%?- prove_online(obl(reagan,reagan and gorbatsjov), [], [obl(neg reagan, true), obl(neg gorbatsjov,
%true), obl(reagan, gorbatsjov), obl(gorbatsjov, reagan)], [],
%[(obl,obl)], [confl(obl,obl)], [], [], derivability,
%'test.tex'). 
%@ true .

/* prove_online
   predicate to be called from the web interface
*/
prove_online(Fml, Facts, D_Assumptions, Sup_Relation, Operators,
	     Inclusions, Conflicts, P_list, [], derivability, Filename) :-
    \+ member(test,Facts),
    prove_with_filename(Fml, Operators, Inclusions, Conflicts, P_list,
			Facts, D_Assumptions, Sup_Relation, Filename).
% for testing with the plandokumente
prove_online(Fml, Facts, D_Assumptions, Sup_Relation, Operators,
	     Inclusions, Conflicts, P_list, Ex_list, derivability, Filename) :-
    phrase(added_facts(Facts,Ex_list),New_Facts),
%    facts(plangebiet(7602),L),
%    append(Facts,L,Facts1),
    phrase(added_assumptions(D_Assumptions,Ex_list),New_D_Assumptions),
%    obligations_plangebiet(plangebiet(7602),O),
%    append(O,D_Assumptions,D_Assumptions1),
    prove_with_filename(Fml, [(obl,obl), (for,for), (per,obl)|Operators], Inclusions, [confl(obl,obl), confl(obl,per), confl(obl,for), confl(for,for), confl(for,per)|Conflicts], P_list,
			New_Facts, New_D_Assumptions, Sup_Relation, Filename).
prove_online(Fml, Facts, D_Assumptions, Sup_Relation, Operators,
	     Inclusions, Conflicts, P_list, _, compliance, Filename) :-
    include_type(obl,Operators,Obligations),
    include_type(for,Operators,Prohibitions),
    preprocess(asmp(Facts, D_Assumptions, ops(Operators, Inclusions
					      , Conflicts, P_list),
		    Sup_Relation), Ass_Processed,Output),
    modalised(Fml,Fml1),
    added_at(Fml1,Fml2),
    maplist(apply_op(Fml2,Fml2), Prohibitions, Proh_list),
    maplist(apply_op(neg Fml2,Fml2), Obligations, Obl_list),
    append(Obl_list,Proh_list,List),
    big_disjunction(List,Disj),
    (prove(Ass_Processed, seq([],[Disj]), Derivation)
    ;
    nonderivable_statement(Derivation)),!,
    phrase(pp_compliance_output(latex,Ass_Processed,Fml2,Disj,Derivation),L),
    atomic_list_concat(L,L1),
    open(Filename,write,Stream),
    write(Stream,L1),
    close(Stream),!.

/* apply_op
*/
apply_op(Fml1,Fml2,Op,modal(Op,Fml1,Fml2)).

/* include_type /3
   true if every operator of type Type from List1 is included in List2
*/
include_type(_,[],[]).
include_type(Type,[(Op,Type)|Tail1],[Op|Tail2]) :-
    include_type(Type,Tail1,Tail2).
include_type(Type,[_|Tail1],Tail2) :-
    include_type(Type,Tail1,Tail2).

/* big_disjunction
   true if the disjunction of the things in the first argument is the
   second argument
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
*/
prove_with_filename(Fml, Operators, Op_Inclusions, Op_Conflicts,
		    Op_P, Facts, D_Assumptions, Sup_Relation, Filename
		   ) :-
    preprocess(asmp(Facts, D_Assumptions, ops(Operators, Op_Inclusions
					      , Op_Conflicts, Op_P),
		    Sup_Relation), Ass_Processed,_),
    modalised(Fml,Fml1),
    added_at(Fml1,Fml2),!, % cut for efficiency
    (prove(Ass_Processed, seq([],[Fml2]), Derivation)
    ;
    nonderivable_statement(Derivation)),!,
    phrase(pp_output(latex,Ass_Processed,Fml2,Derivation),L),
    atomic_list_concat(L,L1),
    open(Filename,write,Stream),
    write(Stream,L1),
    close(Stream),!.


/* prove_test
   to test the prove predicate
*/
/* problematic example:
for(staffelgeschoss, ((plangebiet(7602) and an_oeffentlicher_verkehrsflaeche) and an_strassenfront) and bb(76023))
*/
prove_test(Fml) :-
    prove_online(Fml, [], [], [], [], [], [], [], [plangebiet(7602)], derivability, 'test.tex'),!.

/*prove_test(Fml) :-
    prove(asmp([],[modal(obl,at(a) and at(c),at(b)), modal(obl, neg at(a),at(c)),
		   modal(obl,at(a) and at(d), at(c) and at(d))],
	       ops([(obl,obl)],[obl -> obl],[confl(obl,obl)],[]),[]),
	  seq([],[Fml]),T),
    phrase(pp_output(screen,T),L),
    atomic_list_concat(L,L1),
    write(L1).
*/
prove_test1(Fml) :-
    modalised(Fml,Fml1),
    added_at(Fml1,Fml2),
    prove(asmp([],[],
	       ops([(obl,obl)],[obl -> obl],[confl(obl,obl)],[]),[]),
	  seq([],[Fml2]),T),
/*    phrase(pp_assumptions(screen,asmp([],[],ops([(obl,obl)]
						,[obl -> obl]
						,[confl(obl,obl)],[])
				      ,[])),W),
    atomic_list_concat(W,W1),
    write(W1),
*/
    phrase(pp_output(screen,asmp([],[],
	       ops([(obl,obl)],[obl -> obl],[confl(obl,obl)],[]),[]),Fml2,T),L),
    atomic_list_concat(L,L1),
    write(L1).
prove_test2(Fml,Dass) :-
    modalised(Fml,Fml1),
    added_at(Fml1,Fml2),
    maplist(modalised,Dass,Dass1),
    maplist(added_at,Dass1,Dass2),
    prove(asmp([],Dass2,
	       ops([(obl,obl)],[obl -> obl],[confl(obl,obl)],[]),[]),
	  seq([],[Fml2]),T),
/*    phrase(pp_assumptions(screen,asmp([],[],ops([(obl,obl)]
						,[obl -> obl]
						,[confl(obl,obl)],[])
				      ,[])),W),
    atomic_list_concat(W,W1),
    write(W1),
*/
    phrase(pp_output(screen,asmp([],Dass2,
	       ops([(obl,obl)],[obl -> obl],[confl(obl,obl)],[]),[]),Fml2,T),L),
    atomic_list_concat(L,L1),
    write(L1).

/*    
    prove(asmp([],[norm1:modal(obl,a,b), modal(obl, neg a,c), modal(obl,a, c and d)],ops([(obl,obl)],[obl -> obl],[confl(obl,obl)],[]),[]), seq([],[neg modal(obl,neg a,b and c and d)]),T)
*/
/* prove /3 
   prove(Ass,Seq,Tree) is true if given the assumptions Ass, the
   sequent Seq is provable and the derivation tree is Tree.
 */

/* initial sequents */
prove(_, seq(Gamma, Delta),
      node(botL, seq([false],[]), seq(Gamma,Delta),[])) :-
    member(false, Gamma),!. % green cut for efficiency
prove(_, seq(Gamma, Delta),
      node(topR, seq([],[true]), seq(Gamma,Delta),[])) :-
    member(true, Delta),!. % green cut for efficiency
prove(_, seq(Gamma, Delta),
      node(init, seq([F],[F]), seq(Gamma,Delta),[])) :-
    member(F, Gamma), member(F, Delta),!. % green cut for efficiency

/* factual assumptions */
prove(asmp(Facts,_,_,_), seq(Gamma,Delta), node(fact, seq(Sigma, Pi),
						seq(Gamma, Delta),[])) :-
    member(seq(Sigma,Pi), Facts),
    subset(Sigma,Gamma),
    subset(Pi,Delta),!. % geen cut for efficiency

/* Assumptions about measures
*/
% measure is not greater than max_measure:
prove(_, seq(Gamma,Delta),
      node(fact, seq([at(measure(Type,Object,N)),at(max_measure(Type,Object,M))],[]),
	   seq(Gamma, Delta), [])) :- 
    member(at(measure(Type,Object,N)),Gamma),
    member(at(max_measure(Type,Object,M)),Gamma),
    N > M,!.% green cut for efficiency
% measure is not smaller than min_measure:
prove(_, seq(Gamma,Delta),
      node(fact, seq([at(min_measure(Type,Object,N)),at(measure(Type,Object,M))],[]),
	   seq(Gamma, Delta), [])) :- 
    member(at(measure(Type,Object,N)),Gamma),
    member(at(min_measure(Type,Object,M)),Gamma),
    N < M,!. % green cut for efficiency
% min_measure is monotone:
prove(_, seq(Gamma,Delta),
      node(fact, seq([at(min_measure(Type,Object,N))],[at(min_measure(Type,Object,M))]),
	   seq(Gamma, Delta), [])) :- 
    member(at(min_measure(Type,Object,N)),Gamma),
    member(at(min_measure(Type,Object,M)),Delta),
    M =< N, !. % green cut for efficiency
% max_measure is monotone:
prove(_, seq(Gamma,Delta),
      node(fact, seq([at(max_measure(Type,Object,N))],[at(max_measure(Type,Object,M))]),
	   seq(Gamma, Delta), [])) :- 
    member(at(max_measure(Type,Object,N)),Gamma),
    member(at(max_measure(Type,Object,M)),Delta),
    N =< M, !. % green cut for efficiency
% min_measure is not larger than max_measure:
prove(_, seq(Gamma,Delta),
      node(fact, seq([at(min_measure(Type,Object,N)),at(max_measure(Type,Object,M))],[]),
	   seq(Gamma, Delta), [])) :- 
    member(at(min_measure(Type,Object,N)),Gamma),
    member(at(max_measure(Type,Object,M)),Gamma),
    N > M, !. % green cut for efficiency

      
/* propositional rules */
/* non-branching rules first for efficiency*/
/* negation */
prove(Assumptions, seq(Gamma,Delta),
      node(negL, seq([neg A],[]), seq(Gamma,Delta), [T])) :-
    select(neg A, Gamma, Sigma),
    \+ member(A, Delta),
    \+ member(inv(A), Delta),
    prove(Assumptions, seq([inv(A)|Sigma], [A|Delta]), T),!.% green cut for efficiency
prove(Assumptions, seq(Gamma,Delta),
      node(negR, seq([],[neg A]), seq(Gamma,Delta), [T])) :-
    select(neg A, Delta, Pi),
    \+ member(A, Gamma),
    \+ member(inv(A), Gamma),
    prove(Assumptions, seq([A|Gamma], [inv(A)|Pi]), T),!.% green cut for efficiency

/* conjunction left */
prove(Assumptions, seq(Gamma, Delta),
      node(conjL, seq([A and B],[]), seq(Gamma,Delta), [T])) :-
    select(A and B, Gamma, Sigma),
    ((\+ member(A,Gamma), \+ member(inv(A),Gamma))
    ;(\+ member(B,Gamma), \+ member(inv(B),Gamma))),
    prove(Assumptions, seq([A,B|Sigma],Delta), T),!.% green cut for efficiency

/* disjunction right */
prove(Assumptions, seq(Gamma, Delta),
      node(disjR, seq([],[A or B]), seq(Gamma,Delta), [T])) :-
    select(A or B, Delta, Pi),
    ((\+ member(A,Delta), \+ member(inv(A),Delta))
    ;(\+ member(B,Delta), \+ member(inv(B),Delta))),
    prove(Assumptions, seq(Gamma,[A,B|Pi]), T),!.% green cut for efficiency

/* implication right */
prove(Assumptions, seq(Gamma, Delta),
      node(implR, seq([],[A -> B]), seq(Gamma,Delta), [T])) :-
    select(A -> B, Delta, Pi),
    ((\+ member(A,Gamma), \+ member(inv(A),Gamma))
    ;(\+ member(B,Delta), \+ member(inv(B),Delta))),
    prove(Assumptions, seq([A|Gamma],[B|Pi]), T),!.% green cut for efficiency

/* branching rules */
/* conjunction right */
prove(Assumptions, seq(Gamma, Delta),
      node(conjR, seq([],[A and B]), seq(Gamma, Delta), [T1,T2])) :-
    select(A and B, Delta, Pi),
    \+ member(A, Delta), \+ member(inv(A), Delta),
    \+ member(B, Delta), \+ member(inv(B), Delta),
    prove(Assumptions, seq(Gamma, [A|Pi]), T1),
    prove(Assumptions, seq(Gamma, [B|Pi]), T2),!.% green cut for efficiency

/* disjunction left */
prove(Assumptions, seq(Gamma, Delta),
      node(disjL, seq([A or B],[]), seq(Gamma, Delta), [T1,T2])) :-
    select(A or B, Gamma, Sigma),
    \+ member(A, Gamma), \+ member(inv(A), Gamma),
    \+ member(B, Gamma), \+ member(inv(B), Gamma),
    prove(Assumptions, seq([A|Sigma], Delta), T1),
    prove(Assumptions, seq([B|Sigma], Delta), T2),!.% green cut for efficiency

/* implication left */
prove(Assumptions, seq(Gamma, Delta),
      node(implL, seq([A -> B],[]), seq(Gamma, Delta), [T1,T2])) :-
    select(A -> B, Gamma, Sigma),
    \+ member(A, Delta), \+ member(inv(A), Delta),
    \+ member(B, Gamma), \+ member(inv(B), Gamma),
    prove(Assumptions, seq([B|Sigma], Delta), T1),
    prove(Assumptions, seq(Sigma, [A|Delta]), T2),!.% green cut for efficiency


/* deontic/modal rules */
/* monotonicity rule */
prove(Assumptions, seq(Gamma,Delta),
      node(mon(Op1,Op2), seq([modal(Op1,A,B)],[modal(Op2,C,D)]),
	   seq(Gamma,Delta), [T1, T2, T3] )) :-
    member(modal(Op1,A,B), Gamma),
    member(modal(Op2,C,D), Delta),
    implies(Assumptions,Op1,Op2),
    impl(Assumptions, Op1, Op2, A, C, Seq),
    prove(Assumptions, Seq, T1),
    prove(Assumptions, seq([B],[D]), T2),
    prove(Assumptions, seq([D],[B]), T3).

/* D rule */
prove(Assumptions, seq(Gamma,Delta),
      node(confl(Op1,Op2), seq([modal(Op1,A,B), modal(Op2,C,D)],[]),
	   seq(Gamma,Delta), [T1, T2, T3] )) :-
    member(modal(Op1,A,B), Gamma),
    member(modal(Op2,C,D), Gamma),
    conflicts(Assumptions, Op1, Op2),
    confl(Assumptions, Op1, Op2, A, C, Seq),
    prove(Assumptions, Seq, T1),
    prove(Assumptions, seq([B],[D]), T2),
    prove(Assumptions, seq([D],[B]), T3).

/* P rule */
/* NOTE: for operators with Op confl Op this already is covered by the
 * D rule above! */
/* TODO: [x] add the generic P-predicate here as well */
prove(Assumptions, seq(Gamma,Delta),
      node(pRule(Op), seq([modal(Op,A,B)],[]),
	   seq(Gamma,Delta), [T] )) :-
    member(modal(Op,A,B), Gamma),
    nontrivial(Assumptions, Op),
    confl(Assumptions, Op, Op, A, A, Seq),
    prove(Assumptions, Seq, T).

/* assumption right rule */
prove(asmp(Facts,D_ass_list,Op_char,Rel), seq(Gamma,Delta),
      node(asmpR(Op1,Assumption), seq([],[modal(Op1,A,B)]),
	   seq(Gamma,Delta), [T1,T2,T3,node(not_overruled(Assumption),Tree_list)]))
:-
    member(modal(Op1,A,B), Delta),
    % the formula modal(Op2,C,D) or Norm:modal(Op2,C,D) is in D_ass_list:
    member_norm(Assumption,Op2,C,D,D_ass_list),
    implies(asmp(Facts,D_ass_list,Op_char,Rel),Op2,Op1),
    % the assumption is applicable:
    prove(asmp(Facts,D_ass_list,Op_char,Rel), seq([B],[D]), T1),
    % the condition is implied by the assumption:
    impl(asmp(Facts,D_ass_list,Op_char,Rel), Op2,Op1, C,A, Seq1),
    prove(asmp(Facts,D_ass_list,Op_char,Rel), Seq1, T2),
    % there is no conflict with the P-axiom for Op1 if applicable:
    no_conflict_p(asmp(Facts,D_ass_list,Op_char,Rel),Op1,A,T3),
    % filter out the assumptions for conflicting operators:
    include(applicable_assumption(asmp(Facts,D_ass_list,Op_char,Rel),
			     modal(Op2,C,D)),
	    D_ass_list, Outer_list),
    % HERE TODO [x] change the modal(Op2,C,D) to Assumption
    not_overruled(r,asmp(Facts,D_ass_list,Op_char,Rel),
			  modal(Op1,A,B), Assumption, Outer_list,
	    Tree_list).
	    
/* assumption left rule */
prove(asmp(Facts,D_ass_list,Op_char,Rel), seq(Gamma,Delta),
      node(asmpL(Op1,Assumption), seq([modal(Op1,A,B)],[]),
	   seq(Gamma,Delta), [T1,T2,node(not_overruled(Assumption),Tree_list)]))
:-
    member(modal(Op1,A,B), Gamma),
    member_norm(Assumption,Op2,C,D,D_ass_list),
    conflicts(asmp(Facts,D_ass_list,Op_char,Rel),Op1,Op2),
    % the assumption is applicable:
    prove(asmp(Facts,D_ass_list,Op_char,Rel), seq([B],[D]), T1),
    % the condition is in conflict with the assumption:
    confl(asmp(Facts,D_ass_list,Op_char,Rel), Op2,Op1, C,A, Seq1),
    prove(asmp(Facts,D_ass_list,Op_char,Rel), Seq1, T2),
    % filter out the assumptions for conflicting operators:
    include(applicable_assumption(asmp(Facts,D_ass_list,Op_char,Rel),
				  modal(Op2,C,D)), D_ass_list,
	    Outer_list),
/*  OLD:  include(applicable_outer(asmp(Facts,D_ass_list,Op_char,Rel),
			     modal(Op1,A,B),modal(Op2,C,D)),
	    D_ass_list, Outer_list),
*/
    % HERE: TODO [x] change the modal(Op2,C,D) to Assumption
    not_overruled(l,asmp(Facts,D_ass_list,Op_char,Rel),
			  modal(Op1,A,B), Assumption, Outer_list,
	    Tree_list).
	    

/* applicable_outer
   predicate for filtering out the deontic assumptions which should be
   included in the outer set in the assumption rules.
   Takes as argument the side of the assumption rule.
*/
% TODO: [x] make the "beaten by" into something which is proven, so that
% we can display it properly in the pretty printing! (i.e., put it
% into the "not_overruled" instead!)
/*
applicable_outer(Assumptions,modal(Op1,_,_), modal(Op2,C,D), Fml3)
:- 
    modal_arguments(Fml3,Op3,E,F),
    conflicts(Assumptions,Op3,Op1),
    nbeats(Assumptions,modal(Op2,C,D),modal(Op3,E,F)).
*/
/*
applicable_outer(l,Assumptions,modal(Op1,_,_), modal(Op2,C,D),
		 Fml3) :-
    modal_arguments(Fml3,Op3,E,F),
    implies(Assumptions,Op3,Op1),
    nbeats(Assumptions,modal(Op2,C,D),modal(Op3,E,F)).
applicable_outer(l,Assumptions,modal(Op1,_,_), modal(Op2,C,D),
		 Fml3) :-
    modal_arguments(Fml3,per(Op2),E,F),
    nbeats(Assumptions,modal(Op2,C,D),modal(per(Op2),E,F)).
applicable_outer(r,Assumptions,modal(Op1,_,_), modal(Op2,C,D),
		 Fml3) :-
    modal_arguments(Fml3,Op3,E,F),
    conflicts(Assumptions,Op3,Op1),
    nbeats(Assumptions,modal(Op2,C,D),modal(Op3,E,F)).
*/

/* not_overruled
   The assumption modal(Op2,C,D) is not overruled by a more specific
   or conflicting undefeated assumption modal(Op3,E,F) from a list of
   possibly conflicting assumptions.
   Takes the side of the assumption rule as the first argument.
*/
not_overruled(_,_,_,_,[],[]).
% clause for not applicable (condition of the possibly conflicting
% modal(Op3,E,F) is not implied by B)
not_overruled(Side,Asmp, modal(Op1,A,B), Assumption
	      , [Fml3|Tail_ass], [node(notapplicable(Fml3,seq([B],[F])
						    ))|Tail_tree]) :-
    modal_arguments(Fml3,_,_,F),
    \+ prove(Asmp, seq([B],[F]), _),
    not_overruled(Side,Asmp, modal(Op1,A,B), Assumption, Tail_ass, Tail_tree).
% clause for no conflict (no conflict between the possibly conflicting
% modal(Op3,E,F) and A
not_overruled(r,Asmp, modal(Op1,A,B), Assumption
	      , [Fml3|Tail_ass], [node(noconflict(Fml3,Seq))|
	      Tail_tree]) :-
    modal_arguments(Fml3,Op3,E,_),
    confl(Asmp,Op3,Op1,E,A,Seq),
    \+ prove(Asmp, Seq, _),
    not_overruled(r,Asmp, modal(Op1,A,B), Assumption,Tail_ass, Tail_tree).
not_overruled(l,Asmp, modal(Op1,A,B), Assumption
	      , [Fml3|Tail_ass], [node(notimplied(Fml3,Seq))|
	      Tail_tree]) :-
    modal_arguments(Fml3,Op3,E,_),
    impl(Asmp,Op3,Op1,E,A,Seq),
    \+ prove(Asmp, Seq, _),
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
    \+ prove(asmp(Facts,D_ass,Op_char,Rel), seq([F],[D]),_),
    % modal(Op3,E,F) is overridden by another more specific assumption:
    include(applicable_assumption(asmp(Facts,D_ass,Op_char,Rel),
				  modal(Op3,E,F)), D_ass, Inner_list),
/* OLD: include(applicable_inner(asmp(Facts,D_ass,Op_char,Rel),
			     modal(Op1,A,B), modal(Op3,E,F)),D_ass,Inner_list),
*/
    overridden(Side,asmp(Facts,D_ass,Op_char,Rel), modal(Op1,A,B),
	       Assumption, Fml3, Inner_list, T),
    not_overruled(Side,asmp(Facts,D_ass,Op_char,Rel), modal(Op1,A,B),
		  Assumption,Tail_ass, Tail_tree).


/* overridden
   true if the formula modal(Op3,E,F) is overridden by a formula from
   the list of relevant deontic assumptions.
   Takes the side of the assumption rule as the first argument
*/
/*
TODO: [x] make sure the overriding one is not beated by the original
          one.
*/
overridden(r,Asmp,  modal(Op1,A,B), _, Fml3
	   , [Fml4|_],
	   node(overrides(Fml4, Fml3),[T1,T2,T3]))
:-
    modal_arguments(Fml3,Op3,E,F),
    modal_arguments(Fml4,Op4,X,Y),
    nbeats(Asmp,modal(Op3,E,F),modal(Op4,X,Y)),
    prove(Asmp,seq([B],[Y]),T1),
    prove(Asmp,seq([Y],[F]),T2),
    impl(Asmp,Op4,Op1,X,A,Seq),
    prove(Asmp,Seq,T3).
overridden(l,Asmp,  modal(Op1,A,B), _, Fml3
	   , [Fml4|_],
	   node(overrides(Fml4, Fml3),[T1,T2,T3]))
:-
    modal_arguments(Fml3,Op3,E,F),
    modal_arguments(Fml4,Op4,X,Y),
    nbeats(Asmp,modal(Op3,E,F),modal(Op4,X,Y)),
    prove(Asmp,seq([B],[Y]),T1),
    prove(Asmp,seq([Y],[F]),T2),
    confl(Asmp,Op4,Op1,X,A,Seq),
    prove(Asmp,Seq,T3).
overridden(Side,Asmp, F1, F2, F3, [_|Tail_list], Tree) :-
    overridden(Side,Asmp, F1,F2,F3, Tail_list, Tree).


/* applicable_assumption
   true if given the assumptions in the first argument, the
   particular deontic assumption in the second argument might be
   overruled by the deontic assumption in the last argument.
*/
applicable_assumption(Assumptions,modal(Op,_,_), Fml) :-
    modal_arguments(Fml,Op2,_,_),
    conflicts(Assumptions,Op2,Op).
%    nbeats(Assumptions,modal(Op,A,B),modal(Op2,C,D)).


/* no_conflict_p
*/
no_conflict_p(Asmp,Op,A,node(no_p_conflict(Op,Seq))) :-
    nontrivial(Asmp,Op),
    confl(Asmp,Op,Op,A,A,Seq),
    \+ prove(Asmp,Seq,_).
no_conflict_p(Asmp,Op,_,node(no_p_conflict(na))) :-
    \+ nontrivial(Asmp,Op).
		  

/* applicable_inner
   true if the formula modal(Op,_,_) should be included in the inner
   list of a rule.
   Takes the side of the assumption rule as the first argument
*/
% TODO: [x need to add the permissions here as well!
% CHECK: this should be comparing Fml3 to Fml2 instead of Fml1?!
/*applicable_inner(Assumptions, modal(Op1,A,B), %modal(Op2,C,D),
		 modal(Op3,E,F), Fml4) :-
    modal_arguments(Fml4,Op4,X,Y),
    conflicts(Assumptions,Op4,Op3),
    nbeats(Assumptions,modal(Op3,E,F), modal(Op4,X,Y)).
*/
/*
applicable_inner(r,_, modal(Op1,_,_), _, Fml) :-
    modal_arguments(Fml,Op1,_,_).
applicable_inner(r,Assumptions, modal(Op1,_,_), Fml3,
		 Fml4) :- 
    modal_arguments(Fml3,Op3,E,F),
    modal_arguments(Fml4,Op4,X,Y),
    implies(Assumptions,Op4,Op1),
    nbeats(Assumptions,modal(Op3,E,F),modal(Op4,X,Y)).
applicable_inner(r,Assumptions, modal(Op1,_,_), Fml3,
		 Fml4) :- 
    modal_arguments(Fml3,Op3,_,-),
    modal_arguments(Fml4,per(Op3),X,Y),
    nbeats(Assumptions,modal(Op3,E,F),modal(per(Op3),X,Y)).
applicable_inner(l,Assumptions, modal(Op1,_,_), Fml3,
		 Fml4) :-
    modal_arguments(Fml3,Op3,E,F),
    modal_arguments(Fml4,Op4,X,Y),
    conflicts(Assumptions, Op4, Op1),
    nbeats(Assumptions,modal(Op3,E,F),modal(Op4,X,Y)).
*/
	   

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
beats(asmp(_,D_assumptions,_,Sup_rel),F1,F2) :-
    member((Norm1:F1),D_assumptions),
    member((Norm2:F2),D_assumptions),
    member(Norm1 beats Norm2, Sup_rel).

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
   true if the 
*/
modal_arguments(modal(Op,A,B),Op,A,B).
modal_arguments((_:modal(Op,A,B)),Op,A,B).
