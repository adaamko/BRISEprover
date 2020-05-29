/*
Copyright 2020 Bjoern Lellmann

    This file is part of deonticProver 1.3.

    deonticProver 1.3 is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    deonticProver 1.3 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with deonticProver 1.3.  If not, see <http://www.gnu.org/licenses/>.
*/

/* prettyprinting.pl
 * contains the predicates used for printing on screen and for writing
 * latex into the output file.
*/

  :- use_module(library(lists)).

/* nonderivable_statement
*/
nonderivable_statement(nonderivable).


/* pp_output
   DCG for producing the output.
*/
pp_output(Format,Assumptions,Formula,Derivation) -->
    pp_header(Format,Assumptions,Formula),
    pp_result(Format,Derivation),
    pp_footer(Format).

pp_compliance_output(Format,Assumptions,Formula,Disj,Derivation) -->
    pp_header(Format,Assumptions,Formula),
    pp_compliance_result(Format,Disj,Derivation),
    pp_footer(Format).

pp_compliance_result(screen,_,nonderivable) -->
    ['The input complies with the noms!'].
pp_compliance_result(screen,_,Derivation) -->
    ['The input does not comply with the norms because:'],
    pp_result(screen,Derivation).
pp_compliance_result(latex,Disj,nonderivable) -->
    pp_nl_tab(0),
    ['\\begin{center}Result: The input complies with the
    norms, because we cannot derive\\\\'],
%    pp_nl_tab(0), ['\\['],
    pp_nl_tab(0), ['\\begin{adjustbox}{max width=\\textwidth}'],
    pp_nl_tab(0), ['$'],pp_Fml(latex,Disj),['$'],
    pp_nl_tab(0),
    ['\\end{adjustbox}'],
    pp_nl_tab(0),['\\end{center}'].
%    pp_nl_tab(0), ['\\]'].
pp_compliance_result(latex,_,node(Rule,PF,Seq,Suc)) -->
    pp_nl_tab(0),
    ['\\begin{center}Result: The input does not comply with the norms,
    because:\\end{center}'],
    pp_nl_tab(0),
    ['\\['], pp_nl_tab(0),
    ['\\begin{adjustbox}{max width=\\textwidth}'],
    pp_nl_tab(0),
    pp_derivation(latex,2,node(Rule,PF,Seq,Suc)),
    pp_nl_tab(0),
    ['\\end{adjustbox}'],pp_nl_tab(0),['\\]'],pp_nl_tab(0).


pp_result(screen,Derivation) --> pp_derivation(screen,0,Derivation).
pp_result(html,Derivation) --> pp_derivation(html,0,Derivation).
pp_result(latex,nonderivable) -->
    pp_nl_tab(0),
    ['\\begin{center}Result: Not derivable!\\end{center}'].
pp_result(latex,node(Rule,PF,Seq,Suc)) -->
    pp_nl_tab(0),
    ['\\begin{center}Result: Derivable!\\end{center}'],
    pp_nl_tab(0),
    ['\\['], pp_nl_tab(0),
    ['\\begin{adjustbox}{max width=\\textwidth}'],
    pp_nl_tab(0),
    pp_derivation(latex,2,node(Rule,PF,Seq,Suc)),
    pp_nl_tab(0),
    ['\\end{adjustbox}'],pp_nl_tab(0),['\\]'],pp_nl_tab(0).



/* pp_header
   DCG for producing the header depending on the format
*/
pp_header(screen,Assumptions, Fml) -->
    pp_nl_tab(0),
    ['The assumptions are:'],
    pp_assumptions(screen,Assumptions),
    pp_nl_tab(0),pp_nl_tab(0),
    ['The input formula is: '],pp_Fml(screen,Fml),pp_nl_tab(0).
pp_header(latex,asmp(Facts, D_ass, ops(Op_list, Incl, Confl, P_list),
		     Relation),Fml) -->
    ['\\documentclass{article}'], pp_nl_tab(0),
    ['\\usepackage{header}'], pp_nl_tab(0),
    ['\\begin{document}'], pp_nl_tab(0),
    ['\\begin{center}'], pp_nl_tab(0),
    ['Facts: $'],
    pp_Seq_list(latex,Facts),
    ['\\,$'], pp_nl_tab(0), pp_nl_tab(0),
    ['Deontic assumptions: $'],
    pp_Fml_list(latex,D_ass),
    ['\\,$'], pp_nl_tab(0), pp_nl_tab(0),
    ['Operators: $'],
    pp_Fml_list(latex,Op_list),
    ['\\,$'], pp_nl_tab(0), pp_nl_tab(0),
    ['Operator inclusions: $'],
    pp_Fml_list(latex,Incl),
    ['\\,$'], pp_nl_tab(0), pp_nl_tab(0),
    ['Operator conflicts: $'],
    pp_Fml_list(latex,Confl),
    ['\\,$'], pp_nl_tab(0), pp_nl_tab(0),
    ['Nontrivial operators: $'],
    pp_Fml_list(latex,P_list),
    ['\\,$'], pp_nl_tab(0), pp_nl_tab(0),
    ['Superiority relation: $'],
    pp_Fml_list(latex,Relation),
    ['\\,$'], pp_nl_tab(0), pp_nl_tab(0),
    ['Input formula: $'],
    pp_Fml(latex,Fml),
    ['$'], pp_nl_tab(0),
    ['\\end{center}'].
pp_header(html,_,_) --> [].

/* pp_footer
   DCG for producgin the footer depending on the format
*/
pp_footer(screen) --> [].
pp_footer(latex) --> pp_nl_tab(0), ['\\end{document}'].
pp_footer(html) --> [].


/* pp_Op /2
   DCG for printing an operator
*/
pp_Op(screen,per(Op)) -->
    ['per['],[Op],[']'].
pp_Op(screen,Op) --> [Op].
% clauses for latex
pp_Op(latex,neg) --> ['\\neg'].
pp_Op(latex,and) --> ['\\land'].
pp_Op(latex,or) --> ['\\lor'].
pp_Op(latex,->) --> ['\\to'].
pp_Op(latex,per(Op)) -->
    ['\\Per{'],pp_Op(latex,Op),['}'].
pp_Op(latex,Op) -->
    {replace_underscores(Op,Op_new)},
    ['\\mathsf{'],[Op_new],['}'].

/* pp_norm
*/
pp_norm(screen,Norm) --> [Norm].
pp_norm(html,Norm) --> [Norm].
pp_norm(latex,Norm) -->
    {replace_underscores(Norm,Norm_new)},
    ['\\texttt{'],[Norm_new],['}'].

/* pp_type
*/
pp_type(screen,Type) --> [Type].
pp_type(html,Type) --> [Type].
pp_type(latex,Type) -->
    {replace_underscores(Type,Type_new)},
    ['\\mathsf{'],[Type_new],['}'].

/* pp_Fml /2
 * DCG to write a formula. Takes additional argument Form for the
 * format (either 'screen', 'latex' or 'html').
*/
% clauses for html: 
pp_Fml(html,at(X)) --> {atom(X)}, [X].
pp_Fml(html,at(X)) --> {\+ atom(X), term_to_atom(X,Y)}, [Y].
pp_Fml(html,true) --> ['true'].
pp_Fml(html,false) --> ['false'].
pp_Fml(html,and(A,B)) -->
    ['('], pp_Fml(html,A),
    [') and ('],
    pp_Fml(html,B), [')'].
pp_Fml(html,or(A,B)) -->
    ['('], pp_Fml(html,A),
    [') or ('],
    pp_Fml(html,B), [')'].
pp_Fml(html,->(A,B)) -->
    ['if ('], pp_Fml(html,A),
    ['), then ('],
    pp_Fml(html,B), [')'].
pp_Fml(html,neg(A)) -->
    ['not ('],
    pp_Fml(html,A),
    [')'].
pp_Fml(html,modal(Op,A,B)) -->
    pp_Op(html,Op),['('],
    pp_Fml(html,A),
    ['), given that ('],
    pp_Fml(html,B), [')'].
pp_Fml(html,(Norm:Fml)) -->
    pp_norm(html,Norm),[':'],pp_Fml(html,Fml).
pp_Fml(html,(Norm1 beats Norm2)) -->
    pp_norm(html,Norm1),[' beats '],pp_norm(html,Norm2).
pp_Fml(html,seq(L,N)) -->
    ['It we assume that '], pp_Fml_list(html,L), 
    [', then it follows that  '], pp_Fml_list(html,N).
% TODO: [ ] MISSING: types, conflicts, inclusions, p_list!
% clauses for latex:
pp_Fml(latex,false) --> ['\\bot'].
pp_Fml(latex,true) --> ['\\top'].
pp_Fml(latex,at(X)) --> {atom(X), replace_underscores(X,Y)},['\\texttt{', Y, '}'].
pp_Fml(latex,at(X)) --> {\+ atom(X), term_to_atom(X,Z),
			  replace_underscores(Z,Y)}
			 ,['\\texttt{', Y, '}'].
pp_Fml(latex, neg(X)) -->
    [' \\neg '], pp_Fml(latex, X).
pp_Fml(latex, and(A,B)) --> 
    ['('], pp_Fml(latex, A), [' \\land '],
    pp_Fml(latex, B), [')'].
pp_Fml(latex, or(A,B)) --> 
    ['('], pp_Fml(latex, A), [' \\lor '],
    pp_Fml(latex, B), [')']. 
pp_Fml(latex, ->(A,B)) --> 
    ['('], pp_Fml(latex, A), [' \\to '],
    pp_Fml(latex, B), [')'].
pp_Fml(latex, modal(Op,A,B)) -->
    pp_Op(latex,Op),['('],
    pp_Fml(latex,A),['/'],
    pp_Fml(latex,B),[')'].
pp_Fml(latex,(Norm:Fml)) -->
    pp_norm(latex,Norm),[':'],pp_Fml(latex,Fml).
pp_Fml(latex,(Norm1 beats Norm2)) -->
    pp_norm(latex,Norm1),[' \\beats '],pp_norm(latex,Norm2).
pp_Fml(latex,(Op,Type)) -->
    ['('],pp_Op(latex,Op),[':'],pp_type(latex,Type),[')'].
pp_Fml(latex,nt(Op)) -->
    pp_Op(latex,Op).
pp_Fml(latex,confl(Op1,Op2)) -->
    pp_Op(latex,Op1),[' \\confl '], pp_Op(latex,Op2).
pp_Fml(latex,(Op1 -> Op2)) -->
    pp_Op(latex,Op1),['\\to'],pp_Op(latex,Op2).
% clauses for screen:
pp_Fml(screen,false) --> ['false'].
pp_Fml(screen,true) --> ['true'].
pp_Fml(screen,at(X)) --> {atom(X)},[X].
pp_Fml(screen,at(X)) --> {\+ atom(X), term_to_atom(X,Y)},[Y].
pp_Fml(screen, neg(X)) -->
    ['neg '], pp_Fml(screen, X).
pp_Fml(screen, and(A,B)) --> 
    ['('], pp_Fml(screen, A), [' and '],
    pp_Fml(screen, B), [')'].
pp_Fml(screen, or(A,B)) --> 
    ['('], pp_Fml(screen, A), [' or '],
    pp_Fml(screen, B), [')']. 
pp_Fml(screen, ->(A,B)) --> 
    ['('], pp_Fml(screen, A), [' -> '],
    pp_Fml(screen, B), [')'].
pp_Fml(screen, modal(Op,A,B)) -->
    pp_Op(screen,Op),['('],
    pp_Fml(screen,A),['/'],
    pp_Fml(screen,B),[')'].
pp_Fml(screen,(Norm:Fml)) -->
    [ Norm, ':'],
    pp_Fml(screen,Fml).
pp_Fml(screen,(Norm1 beats Norm2)) -->
    [ Norm1, ' beats ', Norm2 ].
pp_Fml(screen,(Op,Type)) -->
    ['('],pp_Op(screen,Op),[':'],[Type],[')'].
pp_Fml(screen,nt(Op)) -->
    ['nt('],pp_Op(screen,Op),[')'].
pp_Fml(screen,confl(Op1,Op2)) -->
    ['confl('],pp_Op(screen,Op1),[','],pp_Op(screen,Op2),[')'].
pp_Fml(screen,(Op1 -> Op2)) -->
    pp_Op(screen,Op1),['->'],pp_Op(screen,Op2).


/* pp_Fml_list /2
 * DCG to write a list of formulae.
*/
pp_Fml_list(_, []) --> [].
pp_Fml_list(Form, [inv(_)|Tail]) -->
    pp_Fml_list(Form,Tail).
pp_Fml_list(Form, [A|[]]) --> 
    pp_Fml(Form, A).
pp_Fml_list(Form, [A|Tail]) --> 
    pp_Fml(Form, A), [', '], pp_Fml_list(Form, Tail).
/* pp_Fml_list /3
 * DCG for writing a list of formulae in html format, depending on the
 * side of the sequent.
 * The additional argument specifies the left or right hand side of
 * the sequent.
*/
pp_Fml_list(html,l, []) --> ['true'].
pp_Fml_list(html,r, []) --> ['we have a contradiction'].
pp_Fml_list(html,_, [A|[]]) --> 
    pp_Fml(html, A).
pp_Fml_list(html,l, [A|Tail]) --> 
    pp_Fml(html, A), [' and '], pp_Fml_list(html,l, Tail).
pp_Fml_list(html,r, [A|Tail]) --> 
    pp_Fml(html, A), [' or '], pp_Fml_list(html,r, Tail).


/* pp_Seq
 * DCG to print a sequent, with argument specifying whether it is
 * printed on screen, in latex, or as explanation in html.
*/
pp_Seq(latex,seq(A,B)) --> 
    pp_Fml_list(latex,A), 
    pp_Seq_arrow(latex),
    pp_Fml_list(latex,B).
pp_Seq(screen,seq(A,B)) --> 
    pp_Fml_list(screen,A), 
    pp_Seq_arrow(screen), 
    pp_Fml_list(screen,B).
pp_Seq(html,seq(A,B)) --> 
    ['If we assume that ( '], pp_Fml_list(html,l,A),
    ['),<br />
 then it is the case that ( '], pp_Fml_list(html,r,B),[' )'].


/* pp_Seq_arrow
   DCG for printing the sequent arrow in the different formats
*/
pp_Seq_arrow(screen) --> [' => '].
pp_Seq_arrow(latex) --> ['\\seq'].
pp_Seq_arrow(html) --> [' => '].


/* pp_Seq_list
   DCG for printing a list of sequents
*/
pp_Seq_list(screen,[]) --> [].
pp_Seq_list(screen,[Seq|Tail]) -->
    pp_nl_tab(2), pp_Seq(screen,Seq), pp_Seq_list(screen,Tail).
pp_Seq_list(latex,[]) --> [].
pp_Seq_list(latex,[Seq|[]]) -->
    ['\\big( '],pp_Seq(latex,Seq),[' \\big)'].
pp_Seq_list(latex,[Seq1,Seq2|Tail]) -->
    ['\\big( '],pp_Seq(latex,Seq1),[' \\big); '],
    pp_Seq_list(latex,[Seq2|Tail]).

/* pp_assumptions
*/
pp_assumptions(screen,asmp(Facts,D_Ass,ops(Ops,Incl,Confl,Rec),Sup))
-->
    pp_nl_tab(0),
    ['Facts: '], pp_Seq_list(screen,Facts),
    pp_nl_tab(0),
    ['Deontic assumptions: '], pp_Fml_list(screen,D_Ass),
    pp_nl_tab(0),
    ['Operators: '], pp_Fml_list(screen,Ops),
    pp_nl_tab(0),
    ['Operator inclusions: '], pp_Fml_list(screen,Incl),
    pp_nl_tab(0),
    ['Operator conflicts: '], pp_Fml_list(screen,Confl),
    pp_nl_tab(0),
    ['Operator recommendations: '], pp_Fml_list(screen,Rec),
    pp_nl_tab(0),
    ['Superiority relation: '], pp_Fml_list(screen,Sup).
    

/* pp_derivation
   DCG for producing a derivation in latex, html, or on the screen.
   First argument is the format, second argument is indenting depth,
   third argument is the derivation tree.
*/

/* clauses for screen */
pp_derivation(screen,_,nonderivable) -->
    pp_nl_tab(0),['input is not derivable'].
pp_derivation(screen,N,node(init, PF, Seq, _)) -->
    pp_nl_tab(N),
    ['init['],pp_Seq(screen,PF),[']( '],
    pp_Seq(screen,Seq),
    pp_nl_tab(N+1), [')'].
pp_derivation(screen,N,node(botL, _, Seq, _)) -->
    pp_nl_tab(N),
    ['botL( '],
    pp_Seq(screen,Seq),
    pp_nl_tab(N+1), [')'].
pp_derivation(screen,N,node(topR, _, Seq, _)) -->
    pp_nl_tab(N),
    ['topR( '],
    pp_Seq(screen,Seq),
    pp_nl_tab(N+1), [')'].
pp_derivation(screen,N,node(fact, PF, Seq, _)) -->
    pp_nl_tab(N),
    ['fact['], pp_Seq(screen,PF), [']( '],
    pp_Seq(screen,Seq),
    pp_nl_tab(N+1),[')'].
pp_derivation(screen,N,node(Rule,PF,Seq,Suc)) -->
    {rule_type(Rule,propositional)},
    pp_nl_tab(N),
    [Rule],['['],pp_Seq(screen,PF),[']( '],
    pp_Seq(screen,Seq),
    pp_derivation_list(screen,N + 2,Suc),
    pp_nl_tab(N + 1),
    [')'].
% Monotonicity rule:
pp_derivation(screen,N,node(mon(Op1,Op2),PF,Seq,Suc)) -->
    pp_nl_tab(N),
    ['mon('],pp_Op(screen,Op1),[','],pp_Op(screen,Op2),[')['],
    pp_Seq(screen,PF), [']( '],
    pp_Seq(screen,Seq),
    pp_derivation_list(screen,N+2,Suc),
    pp_nl_tab(N+1),[' )'].
% P rule:
pp_derivation(screen,N,node(pRule(Op),PF,Seq,Suc)) -->
    pp_nl_tab(N),
    ['P('],pp_Op(screen,Op),[')['],
    pp_Seq(screen,PF), [']( '],
    pp_Seq(screen,Seq),
    pp_derivation_list(screen,N+2,Suc),
    pp_nl_tab(N+1),[' )'].
% conflict/D rule:
pp_derivation(screen,N,node(confl(Op1,Op2),PF,Seq,Suc)) -->
    pp_nl_tab(N),
    ['D('],pp_Op(screen,Op1),[','],pp_Op(screen,Op2),[')['],
    pp_Seq(screen,PF), [']( '],
    pp_Seq(screen,Seq),
    pp_derivation_list(screen,N+2,Suc),
    pp_nl_tab(N+1),[' )'].
% assumption right rule:
pp_derivation(screen,N,node(asmpR(Op1,Assumption), PF, Seq, Suc)) -->
    pp_nl_tab(N),
    pp_Op(screen,Op1),['-right from '],pp_Fml(screen,Assumption),['['],
    pp_Seq(screen,PF),[']( '],
    pp_Seq(screen,Seq),
    pp_derivation_list(screen,N+2,Suc),
    pp_nl_tab(N+1),[' )'].
% assumption left rule:
pp_derivation(screen,N,node(asmpL(Op1,Assumption), PF, Seq, Suc)) -->
    pp_nl_tab(N),
    pp_Op(screen,Op1),['-left from '],pp_Fml(screen,Assumption),['['],
    pp_Seq(screen,PF),[']( '],
    pp_Seq(screen,Seq),
    pp_derivation_list(screen,N+2,Suc),
    pp_nl_tab(N+1),[' )'].
% clauses for the different blocks in the assumption rules:    
pp_derivation(screen,N,node(no_p_conflict(Op,_))) -->
    pp_nl_tab(N),
    ['The operator '],pp_Op(screen,Op)
    ,[' is non-trivial, but there is no conflict'].
pp_derivation(screen,_,node(no_p_conflict(na))) -->
    [].
pp_derivation(screen,N,node(not_overruled(Assumption),Suc))
-->
    pp_nl_tab(N),
    ['The assumption '],pp_Fml(screen,Assumption),
    [' is not overruled:'],
    pp_derivation_list(screen,N+2,Suc).
pp_derivation(screen,N,node(notapplicable(Fml,_))) -->
    pp_nl_tab(N),
    ['The assumption '],pp_Fml(screen,Fml),[' is not applicable'].
pp_derivation(screen,N,node(noconflict(Fml,_))) -->
    pp_nl_tab(N),
    ['The assumption '],pp_Fml(screen,Fml),[' is not in conflict'].
pp_derivation(screen,N,node(notimplied(Fml,_))) -->
    pp_nl_tab(N),
    ['The assumption '],pp_Fml(screen,Fml)
    ,[' does not imply a conflict'].
pp_derivation(screen,N,node(superior(Norm1:Fml1, Norm2:Fml2))) -->
    pp_nl_tab(N),
    ['The assumption '],pp_Fml(screen,Norm2:Fml2),
    [' is inferior to '],pp_Fml(screen,Norm1:Fml1),
    [', because '],pp_Fml(screen, Norm1 beats Norm2).
pp_derivation(screen,N,node(notoverruled(Fml,_,[Suc]))) -->
    pp_nl_tab(N),
    ['The assumption '],pp_Fml(screen,Fml),
    [' is not more specific than the one we used and'],
    pp_derivation(screen,N,Suc).
pp_derivation(screen,N,node(overrides(Fml1, Fml2),[T1,T2,T3])) -->
    pp_nl_tab(N),
    ['The assumption '],pp_Fml(screen,Fml2),
/*    [' is not more specific than the one we used'],
    pp_nl_tab(N),
*/
    [' is overridden by the assumption '],pp_Fml(screen,Fml1), 
    [' because: '],pp_nl_tab(N+2),
    ['It is applicable:'],
    pp_derivation(screen,N+4,T1),
    pp_nl_tab(N+2),
    ['It is more specific:'],
    pp_derivation(screen,N+4,T2),
    pp_nl_tab(N+2),
    ['It reinstates what we want to derive:'],
    pp_derivation(screen,N+4,T3).

% clauses for latex:
pp_derivation(latex,N,nonderivable) -->
    pp_nl_tab(N),['input is not derivable'].
pp_derivation(latex,N,node(Rule_name, _, Seq, _)) -->
    {member(Rule_name,[init,botL,topR])},
    pp_nl_tab(N),
    ['\\infer[\\'],[Rule_name],[']{'],
    pp_Seq(latex,Seq),['}{} '].
pp_derivation(latex,N,node(fact, PF, Seq, _)) -->
    pp_nl_tab(N),
    ['\\infer[\\fact]{'],
    pp_Seq(latex,Seq), ['}{'],
    pp_Seq(latex,PF),['}'].
% propositional rules:
pp_derivation(latex,N,node(Rule_name,_,Seq,Suc)) -->
    {rule_type(Rule_name,propositional)},
    pp_nl_tab(N),
    ['\\infer[\\'],[Rule_name],[']{'],
    pp_Seq(latex,Seq),['}{'],
    pp_derivation_list(latex,N + 2,Suc),
    pp_nl_tab(N), ['}'].
% Monotonicity rule:
pp_derivation(latex,N,node(mon(Op1,Op2),_,Seq,Suc)) -->
    pp_nl_tab(N),
    ['\\infer[\\Mon_{'],pp_Op(latex,Op1),[','],pp_Op(latex,Op2),['}]{'],
    pp_Seq(latex,Seq),['}{'],
    pp_derivation_list(latex,N+2,Suc),
    pp_nl_tab(N),['}'].
% P rule:
pp_derivation(latex,N,node(pRule(Op),_,Seq,Suc)) -->
    pp_nl_tab(N),
    ['\\infer[\\mathsf{P}_{'],pp_Op(latex,Op),['}]{'],
    pp_Seq(latex,Seq),['}{'],
    pp_derivation_list(latex,N+2,Suc),
    pp_nl_tab(N),['}'].
% conflict/D rule:
pp_derivation(latex,N,node(confl(Op1,Op2),_,Seq,Suc)) -->
    pp_nl_tab(N),
    ['\\infer[\\mathsf{D}_{'],pp_Op(latex,Op1),[','],pp_Op(latex,Op2),['}]{'],
    pp_Seq(latex,Seq),['}{'],
    pp_derivation_list(latex,N+2,Suc),
    pp_nl_tab(N),['}'].
% assumption right rule:
pp_derivation(latex,N,node(asmpR(Op1,Assumption), _, Seq, Suc)) -->
    pp_nl_tab(N),
    ['\\infer['],pp_Op(latex,Op1),['_R^{'],pp_Fml(latex,Assumption),['}]{'],
    pp_Seq(latex,Seq),['}{'],
    pp_derivation_list(latex,N+2,Suc),
    pp_nl_tab(N),['}'].
% assumption left rule:
pp_derivation(latex,N,node(asmpL(Op1,Assumption), _, Seq, Suc)) -->
    pp_nl_tab(N),
    ['\\infer['],pp_Op(latex,Op1),['_L^{'],pp_Fml(latex,Assumption),['}]{'],
    pp_Seq(latex,Seq),['}{'],
    pp_derivation_list(latex,N+2,Suc),
    pp_nl_tab(N),['}'].
% TODO: [ ] add the underivability stuff / blocks for assumption rules
% clauses for the different blocks in the assumption rules:    
pp_derivation(latex,N,node(no_p_conflict(Op,Seq))) -->
    pp_nl_tab(N),
    ['\\begin{array}[b]{l}\\text{We have }\\wconfl'],
    pp_Op(latex,Op),['\\text{ but}\\\\ \\text{no conflict, because:}\\\\ \\nentails'],
    pp_Seq(latex,Seq),['\\end{array}'].
pp_derivation(latex,_,node(no_p_conflict(na))) -->
    [].
pp_derivation(latex,N,node(not_overruled(Assumption),Suc))
-->
    pp_nl_tab(N),
    ['\\begin{array}[b]{l}\\text{The assumption}\\\\'],
    pp_Fml(latex,Assumption),
    ['\\\\ \\text{is not overruled:}\\end{array}'], %pp_Fml(latex,Assumption),
%    [' is not overruled: & '],
    pp_nl_tab(N),
    ['&'],
    pp_derivation_list(latex,N+2,Suc).
pp_derivation(latex,N,node(notapplicable(Fml,Seq))) -->
    pp_nl_tab(N),
    ['\\begin{array}[b]{l}\\text{For }'],
    pp_Fml(latex,Fml),[':\\\\ '],
    ['\\text{not applicable, because}\\\\ \\nentails'],
    pp_Seq(latex,Seq),['\\end{array}'].
pp_derivation(latex,N,node(noconflict(Fml,Seq))) -->
    pp_nl_tab(N),
    ['\\begin{array}[b]{l}\\text{For }'],
    pp_Fml(latex,Fml),[':\\\\ '],
    ['\\text{no conflict, because}\\\\ \\nentails'],
    pp_Seq(latex,Seq),['\\end{array}'].
pp_derivation(latex,N,node(notimplied(Fml,Seq))) -->
    pp_nl_tab(N),
    ['\\begin{array}[b]{l}\\text{For }'],
    pp_Fml(latex,Fml), [':\\\\ '],
    ['\\text{no conflict, because}\\\\ \\nentails'],
    pp_Seq(latex,Seq),['\\end{array}'].
pp_derivation(latex,N,node(superior(Norm1:_, Norm2:Fml2))) -->
    pp_nl_tab(N),
    ['\\begin{array}[b]{l}\\text{For }'],
    pp_Fml(latex,Norm2:Fml2),[':\\\\ '],
    %[' is inferior to '],pp_Fml(latex,Norm1:Fml1),
    pp_Fml(latex, Norm1 beats Norm2),
    ['\\end{array}'].
pp_derivation(latex,N,node(notoverruled(Fml,Seq,[Suc]))) -->
    pp_nl_tab(N),
    ['\\begin{array}[b]{l}\\text{For }'],
    pp_Fml(latex,Fml), [':\\\\ '],
    ['\\text{Not more specific, because}\\\\ \\nentails'],
    pp_Seq(latex,Seq),['\\\\ \\text{and also:}\\end{array}'],
    pp_derivation(latex,N,Suc).
pp_derivation(latex,N,node(overrides(Fml1, Fml2),[T1,T2,T3])) -->
    pp_nl_tab(N),
    ['\\begin{array}[b]{l}\\text{For }'],
    pp_Fml(latex,Fml2), [':\\\\ '],
    ['\\text{Overridden by}\\\\'],
    %['\\begin{array}[b]{l}\\text{Overridden by}\\\\'],
    pp_Fml(latex,Fml1),
    %['\\\\ \\text{ and overridden by: }'],
    pp_nl_tab(N),
    %['and it is overridden by the assumption '],
    %pp_Fml(latex,Fml1), 
    ['\\text{ because:} \\end{array}'],
    pp_nl_tab(N),['&'],
    pp_derivation_list(latex,N+2,[T1,T2,T3]).
/*
    ['It is applicable:'],
    pp_derivation(latex,N+4,T1),
    pp_nl_tab(N+2),
    ['It is more specific:'],
    pp_derivation(latex,N+4,T2),
    pp_nl_tab(N+2),
    ['It reinstates what we want to derive:'],
    pp_derivation(latex,N+4,T3).
*/

		  

/* pp_derivation_list
   DCG for pretty printing a list of derivations
*/
% TODO: [ ] add this.
pp_derivation_list(screen,_,[]) --> [].
pp_derivation_list(screen,N,[Der|[]]) -->
    pp_derivation(screen,N,Der).
pp_derivation_list(screen,N,[Der1,Der2|Tail]) -->
    pp_derivation(screen,N,Der1),
    pp_derivation_list(screen,N,[Der2|Tail]).
pp_derivation_list(latex,_,[]) --> [].
pp_derivation_list(latex,N,[Der|[]]) -->
    pp_derivation(latex,N,Der).
pp_derivation_list(latex,N,[no_p_conflict(na),Der2|Tail]) -->
    pp_derivation_list(latex,N,[Der2|Tail]).
pp_derivation_list(latex,N,[Der1,Der2|Tail]) -->
    pp_derivation(latex,N,Der1),
    pp_nl_tab(N),['&'],
    pp_derivation_list(latex,N,[Der2|Tail]).


/************************/
/* Auxiliary predicates */
/************************/

/* rule_type /2
   true if Rule is of the type given by the second argument
*/
% TODO: [ ] add the P rule
rule_type(Rule, propositional) :-
    member(Rule, [negL, negR, disjL, disjR, conjL, conjR, implL, implR]).
rule_type(mon(_,_), modal).
rule_type(confl(_,_),modal).
rule_type(asmpR(_,_),modal).
rule_type(asmpL(_,_),modal).


/* pp_tab(N)
 * DCG for indenting using N spaces
*/
pp_tab(0) --> [].
pp_tab(N) --> {N =\=0, M is N-1}, [' '], pp_tab(M).

/* pp_nl_tab(N)
   DCG for newline followed by N spaces
*/
pp_nl_tab(N) --> ['
'],pp_tab(N).

/* replace_underscores
   For replacing prefixing underscores with the escape character.
*/
/*
replace_underscores([]) --> [].
replace_underscores([\u0x5f|List]) --> ['\\_'], replace_underscores(List).
replace_underscores(['_'|List]) --> ['\\_'], replace_underscores(List).
replace_underscores([X|List]) --> [X],replace_underscores(List).
*/
replace_underscores(Atom_in,Atom_out) :-
    name(Atom_in,List_in),
    replace_underscores_aux(List_in,List_out),
    name(Atom_out,List_out).

replace_underscores_aux([],[]).
replace_underscores_aux([95|Tail_in],[92,95|Tail_out]) :-
    replace_underscores_aux(Tail_in,Tail_out).
replace_underscores_aux([X|Tail_in],[X|Tail_out]) :-
    \+ X == 95,
    replace_underscores_aux(Tail_in,Tail_out).
