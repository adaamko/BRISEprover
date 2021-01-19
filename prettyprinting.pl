/*
Copyright 2020 Bjoern Lellmann

    This file is part of BRISEprover.

    BRISEprover is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    deonticProver 1.3 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BRISEprover.  If not, see <http://www.gnu.org/licenses/>.
*/

/* prettyprinting.pl
 * contains the predicates used for printing on screen and for writing
 * latex into the output file.
*/

  :- use_module(library(lists)).


/* for testing
*/
html_test(Fml,Deriv) :-
    phrase(pp_output(html,asmp([seq([at(a)],[at(b)])], [bb(2/3):modal(obl,at(c),at(d)), modal(for,at(d), at(e))], ops([], [], [], []),
		     [bb(2/3) beats b(3:3)]),Fml,Deriv),L),
    atomic_list_concat(L,L1),
    open('test.html',write,Stream),
    write(Stream,L1),
    close(Stream),!.
/*Reminder:
    asmp(Facts, Deontic_assumptions, Op_characterisation, Relation)
   where Op_characterisation has the form
     ops(Operator_list, Inclusion_relation, Conflict_relation, P_relation)
*/

/* Test query for pasting into the terminal:
test_ass(A),prove(modern,A,seq([],[modal(obl,at(b),at(c))]),T),tree_vs_named_tree(T,W), phrase(pp_derivation(html,0,W),L), atomic_list_concat(L,L1), open('html_test.html',write,Stream),write(Stream,L1),close(Stream).
*/


/* nonderivable_statement
*/
nonderivable_statement(nonderivable).


/* pp_output//4
   DCG for producing the output of a derivability check.
*/
pp_output(Format,Assumptions,Formula,Derivation) -->
    pp_header(Format,Assumptions,Formula),
    pp_result(Format,Derivation),
    pp_footer(Format).


/* pp_compliance_output//5
   DCG for producing the output of a compliance check
*/
pp_compliance_output(Format,Assumptions,Formula,Disj,Derivation) -->
    pp_header(Format,Assumptions,Formula),
    pp_compliance_result(Format,Disj,Derivation),
    pp_footer(Format).


/* pp_compliance_result//3
   DCG for pretty printing the result of a compliance check
*/
pp_compliance_result(screen,_,nonderivable) -->
    ['The input complies with the norms!'].
pp_compliance_result(screen,_,Derivation) -->
    ['The input does not comply with the norms because:'],
    pp_result(screen,Derivation).
pp_compliance_result(latex,Disj,nonderivable) -->
    pp_nl_tab(0),
    ['\\begin{center}Result: The input complies with the
    norms, because we cannot derive\\\\'],
    pp_nl_tab(0), ['\\begin{adjustbox}{max width=\\textwidth}'],
    pp_nl_tab(0), ['$'],pp_Fml(latex,Disj),['$'],
    pp_nl_tab(0),
    ['\\end{adjustbox}'],
    pp_nl_tab(0),['\\end{center}'].
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


/* pp_result//2
   DCG for pretty printing the result of a derivability check
*/
pp_result(screen,Derivation) --> pp_derivation(screen,0,Derivation).
%pp_result(html,Derivation) --> pp_derivation(html,0,Derivation).
pp_result(html,nonderivable) -->
    ["<div>"], pp_nl_tab(0),
    ["<kbd>Result</kbd>"],pp_nl_tab(0),
    ["<p>The input is not derivable.</p>"],pp_nl_tab(0),
    ["</div>"].
pp_result(html,Derivation) -->
    ["<div>"], pp_nl_tab(0),
    ["<kbd>Result</kbd>"],pp_nl_tab(0),
    ["<p>"],
    pp_derivation(html,2,Derivation),
    ["</p>"],pp_nl_tab(0),
    ["</div>"].
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


/* pp_header//3
   DCG for producing the header depending on the format
*/
/* TODO:
   [ ] add clause for html
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
pp_header(html,asmp(Facts, D_ass, ops(Op_list, Incl, Confl, P_list), Relation),Fml) -->
    ["<!DOCTYPE html>

<head>
  <title>Test page</title>
  
<!--  <link type=\"text/css\" rel=\"stylesheet\" href=\"stylesheet.css\" media=\"screen\" />
-->
  <style>
  .abstract {
    color: rgb(55,55,55);
    display:none;
    width: 100%;
    text-align: left;
    margin:15px;
    }
  body {
    font-family:\"Helvetica Neue\",Helvetica,Arial,sans-serif;
    font-size:14px;
    line-height:1.42857143;
    color:#333;
    background-color:#fff
  }
  kbd {
    padding:2px 4px;
    font-size:90%;
    color:#fff;
    background-color:#333;
    border-radius:3px;
    -webkit-box-shadow:inset 0 -1px 0 rgba(0,0,0,.25);
    box-shadow:inset 0 -1px 0 rgba(0,0,0,.25)
  }
  code {
    padding:2px 4px;
    font-size:90%;
    color:#c7254e;
    background-color:#f9f2f4;
    border-radius:4px
  }
  hr {
    margin-top:20px;
    margin-bottom:20px;
    border:0;
    border-top:1px solid #eee
  }
  </style>
</head>

<body>

  <div>
"],
    pp_nl_tab(2),
    ["<kbd>Input</kbd>"], pp_nl_tab(2),
    ["<p> The input formula is: <br /><code>"], pp_nl_tab(2), 
    pp_Fml(html,Fml),["</code></p>"], pp_nl_tab(2),
    ["<button class=\"button\" onclick=\"myFunction('facts')\">Show
factual assumptions</button>"],
    pp_nl_tab(2),
    pp_nl_tab(2),
    ["<div class=\"abstract\" id=\"facts\">"],pp_nl_tab(2),
    pp_Facts(html,Facts), pp_nl_tab(2),
    ["</div>"],pp_nl_tab(2),
    ["<br />"],pp_nl_tab(2),
/*
    ["<p>The saturated factual assumptions are: <br />"], pp_nl_tab(2),
    pp_Facts(html,Facts), ["</p>"], pp_nl_tab(2),
*/
    ["<button class=\"button\" onclick=\"myFunction('deonticassumptions')\">Show
deontic assumptions</button>"],
    pp_nl_tab(2),
    pp_nl_tab(2),
    ["<div class=\"abstract\" id=\"deonticassumptions\">"],pp_nl_tab(2),
    pp_srauta(html,D_ass), pp_nl_tab(2),
    ["</div>"],pp_nl_tab(2),
    ["<br />"],pp_nl_tab(2),
/*
    ["<p>The deontic assumptions are: <br />"], pp_nl_tab(2),
    pp_srauta(html,D_ass), ["</p>"], pp_nl_tab(2),
*/
    ["<button class=\"button\" onclick=\"myFunction('superiorityrelation')\">Show
superiority relation</button>"],
    pp_nl_tab(2),
    pp_nl_tab(2),
    ["<div class=\"abstract\" id=\"superiorityrelation\">"],pp_nl_tab(2),
    pp_relation(html,Relation), pp_nl_tab(2),
    ["</div>"],pp_nl_tab(2),
/*
    ["<p>The superiority relation is given by: <br />"], pp_nl_tab(2),
    pp_relation(html,Relation), ["</p>"], pp_nl_tab(2),
    ["</div>"], pp_nl_tab(0), pp_nl_tab(0),
*/
    ["<hr>"], pp_nl_tab(0), pp_nl_tab(0).

/*
<button class=\"button\" onclick=\"myFunction('fct"],
    format_name(Name),["_trunc')\">Why does it follow from the above?</button>"],
    pp_nl_tab(N),
    pp_nl_tab(N),
    ["<div class=\"abstract\" id=\"fct"],format_name(Name),["_trunc\">"],
    ['The detailed explanation of the statement above is the following:<br />'],
    pp_html_truncated_new(N + 2,node(Name, Rule, PF, Seq, Suc)),
    pp_nl_tab(N),
    ["</div>"],
*/



/* pp_Facts
 * DCG for printing a list of facts.
*/
pp_Facts(html,[]) --> ["No factual assumptions."].
pp_Facts(html,[Fact|List]) -->
    pp_nl_tab(0),
    ["<ul>"],pp_nl_tab(0),
    pp_fact_list(html,[Fact|List]),
    ["</ul>"].

pp_fact_list(html,[]) --> [].
pp_fact_list(html,[Fact|List]) -->
    pp_nl_tab(0),
    ["<li>"], pp_nl_tab(2),
    ["<code>"], pp_Seq(html,Fact), ["</code>"], pp_nl_tab(0),
    ["</li>"], pp_nl_tab(0),
    pp_fact_list(html,List).

/* pp_srauta
 * DCG for printing a list of deontic assumptions.
*/
pp_srauta(html,[]) --> ["No deontic assumptions."].
pp_srauta(html,[Srauta|List]) -->
    pp_nl_tab(0),
    ["<ul>"],pp_nl_tab(0),
    pp_srauta_list(html,[Srauta|List]),
    ["</ul>"].
% pp_Fml_list_DCG(html,l,A)

pp_srauta_list(html,[]) --> [].
pp_srauta_list(html,[Srauta|List]) -->
    pp_nl_tab(0),
    ["<li>"], pp_nl_tab(2),
    ["<code>"], pp_Fml(html,Srauta), ["</code>"], pp_nl_tab(0),
    ["</li>"], pp_nl_tab(0),
    pp_srauta_list(html,List).

/* pp_relation
 * DCG for printing a list giving the superiority relation.
*/
pp_relation(html,[]) --> ["No relation given."].
pp_relation(html,[Relation|List]) -->
    pp_nl_tab(0),
    ["<ul>"],pp_nl_tab(0),
    pp_relation_list(html,[Relation|List]),
    ["</ul>"].

pp_relation_list(html,[]) --> [].
pp_relation_list(html,[Relation|List]) -->
    pp_nl_tab(0),
    ["<li>"], pp_nl_tab(2),
    ["<code>"], pp_Fml(html,Relation), ["</code>"], pp_nl_tab(0),
    ["</li>"], pp_nl_tab(0),
    pp_relation_list(html,List).



/* pp_footer//1
   DCG for producing the footer depending on the format
*/
/* TODO
   [x] add clause for html
*/
pp_footer(screen) --> [].
pp_footer(latex) --> pp_nl_tab(0), ['\\end{document}'].
pp_footer(html) --> ["

  <script>
    function myFunction(currDiv) {
        var x = document.getElementById(currDiv);
        if (x.style.display === 'block') {
            x.style.display = 'none';
        } else {
            x.style.display = 'block';
        }
    }
  </script>

</body>"].



/* pp_Op//2
   DCG for pretty printing an operator
*/
% clauses for printing on screen
pp_Op(screen,per(Op)) -->
    ['per['],[Op],[']'].
pp_Op(screen,Op) --> [Op].
% clauses for printing in latex
pp_Op(latex,neg) --> ['\\neg'].
pp_Op(latex,and) --> ['\\land'].
pp_Op(latex,or) --> ['\\lor'].
pp_Op(latex,->) --> ['\\to'].
pp_Op(latex,per(Op)) -->
    ['\\Per{'],pp_Op(latex,Op),['}'].
pp_Op(latex,Op) -->
    {replace_underscores(Op,Op_new)},
    ['\\mathsf{'],[Op_new],['}'].
% clauses for printing in html
pp_Op(html,obl) -->
    ['it should be the case that '].
pp_Op(html,for) -->
    ['it is forbidden that '].
pp_Op(html,for) -->
    ['it is permitted that '].
pp_Op(html,rec) -->
    ['it is recommended that '].
pp_Op(html,per) -->
    ['it is permitted that '].
pp_Op(html,Op) -->
    {replace_underscores(Op,Op_new)},
    ['it is '],[Op_new],[' that '].


/* pp_norm//2
   DCG for pretty printing a norm statement
*/
pp_norm(screen,Norm) --> [Norm].
/*
pp_norm(html,Norm) --> [Norm].
*/
pp_norm(html,Norm) -->
    {atom(Norm)},[Norm].
pp_norm(html,Norm) -->
    {\+ atom(Norm), term_to_atom(Norm,Norm1)},
    [Norm1].
pp_norm(latex,Norm) -->
    {atom(Norm),replace_underscores(Norm,Norm_new)},
    ['\\texttt{'],[Norm_new],['}'].
pp_norm(latex,Norm) -->
    {\+ atom(Norm), term_to_atom(Norm,Norm1),
     replace_underscores(Norm1,Norm_new)}, 
    ['\\texttt{'],[Norm_new],['}'].
/*
pp_norm(latex,bb(A)) -->
    {replace_underscores(A,A_new)},
    ['\\texttt{bb('],[A_new],[')}'].
*/

/* pp_type//2
   DCG for pretty printing an operator type
*/
pp_type(screen,Type) --> [Type].
pp_type(html,obl) --> [' obligation type '].
pp_type(html,for) --> [' prohibition type '].
pp_type(latex,Type) -->
    {replace_underscores(Type,Type_new)},
    ['\\mathsf{'],[Type_new],['}'].


/* pp_Fml//2
   DCG to pretty print a formula. Takes additional argument for the
   format (either 'screen', 'latex' or 'html').
*/
% clauses for html: 
pp_Fml(html,at(measure(X,Y,N))) -->
    ['the '],[Y],[' of a '],[X],[' is '],[N].
pp_Fml(html,at(min_measure(X,Y,N))) -->
    ['the '],[Y],[' of a '],[X],[' is at least '],[N].
pp_Fml(html,at(max_measure(X,Y,N))) -->
    ['the '],[Y],[' of a '],[X],[' is at most '],[N].
pp_Fml(html,at(plangebiet(X))) -->
    {term_to_atom(X,Y)},
    ['the Plangebiet is '],[Y].
pp_Fml(html,at(bauland(X))) -->
    {term_to_atom(X,Y)},
    ['the Bauland is '],[Y].
pp_Fml(html,at(grundflaeche(X))) -->
    {term_to_atom(X,Y)},
    ['the Grundfl&auml;che is '],[Y].
pp_Fml(html,at(baulinie(X))) -->
    {term_to_atom(X,Y)},
    ['the Baulinie is '],[Y].
pp_Fml(html,at(baufluchtlinie(X))) -->
    {term_to_atom(X,Y)},
    ['the Baufluchtlinie is '],[Y].
pp_Fml(html,at(grenzlinie(X))) -->
    {term_to_atom(X,Y)},
    ['the Grenzlinie is '],[Y].
pp_Fml(html,at(widmung(X))) -->
    ['the Widmung is '],[X].
pp_Fml(html,at(bauklasse(X))) -->
    ['the Bauklasse is '],[X].
pp_Fml(html,at(bauweise(X))) -->
    ['the Bauweise is '],[X].
pp_Fml(html,at(bb(X))) -->
    {term_to_atom(X,Y)},
    ['the Besondere Bestimmung '],[Y],[' applies'].
pp_Fml(html,at(b(X))) -->
    {term_to_atom(X,Y)},
    ['the Bestimmung '],[Y],[' applies'].
/* Proper Merkmale */
/* NOTE: 
   Did not add the Merkmale with text as argument, because the meaning
   is not clear.
*/
/* without argument: */
pp_Fml(html,at(kleinhaeuser)) -->
    ['the buildings are Kleinh&auml;ser'].
pp_Fml(html,at(flaecheBebaubar)) -->
    ['the Fl&auml;che is bebaubar'].
pp_Fml(html,at(flaecheBebaut)) -->
    ['the Fl&auml;che is bebaut'].
pp_Fml(html,at(anBaulinie)) -->
    ['the location is on a Baulinie'].
pp_Fml(html,at(anFluchtlinie)) -->
    ['the location is on a Fluchtlinie'].
pp_Fml(html,at(anOeffentlichenVerkehrsflaechen)) -->
    ['the location is an einer &ouml;ffentlichen Verkehrsfl&auml;che'].
pp_Fml(html,at(inSchutzzone)) -->
    ['the location is in einer Schutzzone'].
pp_Fml(html,at(plangebietAllgemein)) -->
    ['the location is in the Plangebiet allgemein'].
pp_Fml(html,at(struktureinheitBebaubar)) -->
    ['the location is in the bebaubare Struktureinheit'].
pp_Fml(html,at(ausnahmePruefungErforderlich)) -->
    ['there is an exception (manual check required!)'].
pp_Fml(html,at(strittigeBedeutung)) -->
    ['the meaning of this regulation is debated'].
pp_Fml(html,at(weitereBestimmungPruefungErforderlich)) -->
    ['there is a regulation requiring a manual check'].
pp_Fml(html,at(anlageZumEinstellenVorhanden)) -->
    ['there is a Anlage zum Einstellen von Kraftfahrzeugen'].
pp_Fml(html,at(stellplatzregulativVorhanden)) -->
    ['a Stellplatzregulativ applies'].
pp_Fml(html,at(gaertnerischeAusgestaltung)) -->
    ['g&auml;rtnerische Ausgestaltung'].
pp_Fml(html,at(fensterZuOeffentlichenVerkehrsflaechen)) -->
    ['there are Fenster zu &ouml;ffentlichen Verkehrsfl&auml;chen'].
pp_Fml(html,at(bebauung)) -->
    ['there is Bebauung'].
pp_Fml(html,at(wohnung)) -->
    ['there is a Wohnung'].
pp_Fml(html,at(gebaeudeP)) -->
    ['there is a Geb&auml;ude f&uuml;r Fl&auml;chen zum Abstellen von Kaftfahrzeugen'].
pp_Fml(html,at(vorbauten)) -->
    ['there are Vorbauten'].
pp_Fml(html,at(errichtungGebaeude)) -->
    ['Errichtung von Geb&auml;uden'].
pp_Fml(html,at(staffelung)) -->
    ['there is a Staffelung der Baumasse'].
pp_Fml(html,at(technischeUndBelichtungsAufbauten)) -->
    ['there are technische bzw. der Belichtung dienende Aufbauten auf dem Dach im
   erforderlichen Ausmass'].
pp_Fml(html,at(technischeAufbauten)) -->
    ['there are technische Aufbauten'].
pp_Fml(html,at(unterbrechungGeschlosseneBauweise)) -->
    ['there is a Unterbrechung der geschlossenen Bauweise'].
pp_Fml(html,at(unterirdischeBauwerke)) -->
    ['there are unterirdische Bauwerke'].
pp_Fml(html,at(unterirdischeBaulichkeiten)) -->
    ['there are unterirdische Baulichkeiten'].
pp_Fml(html,at(einfriedung)) -->
    ['there is an Einfriedung'].
pp_Fml(html,at(hochhausGemaessBB)) -->
    ['there is a Hochhaus'].
pp_Fml(html,at(bueroGeschaeftsgebaeude)) -->
    ['there is a B&uuml;ro oder Gesch&auml;ftsgeba&auml;ude'].
pp_Fml(html,at(aufenthaltsraum)) -->
    ['there is a Aufenthaltsraum'].
pp_Fml(html,at(wohnung)) -->
    ['there is a Wohnung'].
pp_Fml(html,at(stellplatzImNiveau)) -->
    ['there is a Stellplatz im Niveau'].
/* with an argument: */
pp_Fml(html,at(bauweiseID(X))) -->
    ['the Bauweise is '],[X].
pp_Fml(html,at(gebaeudeBautyp(X))) -->
    ['the Bautyp is '],[X].
pp_Fml(html,at(dachart(X))) -->
    ['the Dachart is '],[X].
pp_Fml(html,at(vorkehrungBepflanzungOeffentlicheVerkehrsflaeche(X))) -->
    ['there is the provision to plant '],[X],[' on a public traffic area'].
pp_Fml(html,at(stockwerk(X))) -->
    ['the Stockwerk is '],[X].
pp_Fml(html,at(bauklasseID(X))) -->
    ['the Bauklasse is '],[X].
pp_Fml(html,at(gebaeudeHoeheArt(X))) -->
    ['the kind of building height is '],[X].
pp_Fml(html,at(planzeichenBBID(X))) -->
    ['the Planzeichen is '],[X].
pp_Fml(html,at(widmungID(X))) -->
    ['the Widmung is '],[X].
pp_Fml(html,at(widmungErsteEbene(X))) -->
    ['the Widmung of the first level is '],[X].
pp_Fml(html,at(widmungZweiteEbene(X))) -->
    ['the Widmung of the second level is '],[X].
pp_Fml(html,at(widmungDritteEbene(X))) -->
    ['the Widmung of the third level is '],[X].
pp_Fml(html,at(widmungErsteEbeneBezugHoehe(X))) -->
    ['the Widmung of the first level starts at a height of '],[X].
pp_Fml(html,at(widmungZweiteEbeneBezugHoehe(X))) -->
    ['the Widmung of the second level starts at a height of '],[X].
pp_Fml(html,at(widmungDritteEbeneBezugHoehe(X))) -->
    ['the Widmung of the third level starts at a height of '],[X].
/* measures */
% anteilBaumbepflanzung
pp_Fml(html,at(anteilBaumbepflanzungGenau(X))) -->
    ['the Anteil der Baumbepflanzung is exactly '],[X],[' percent'].
pp_Fml(html,at(anteilBaumbepflanzung(X))) -->
    ['the Anteil der Baumbepflanzung is at least '],[X],[' percent'].
pp_Fml(html,at(anteilBaumbepflanzungMax(X))) -->
    ['the Anteil der Baumbepflanzung is at most '],[X],[' percent'].
% technischeAufbautenHoehe
pp_Fml(html,at(technischeAufbautenHoeheGenau(X))) -->
    ['the height of technische Aufbauten is exactly '],[X],[' centimeters'].
pp_Fml(html,at(technischeAufbautenHoeheMin(X))) -->
    ['the height of technische Aufbauten is at least '],[X],[' centimeters'].
pp_Fml(html,at(technischeAufbautenHoeheMax(X))) -->
    ['the height of technische Aufbauten is at most '],[X],[' centimeters'].
% abschlussDach
pp_Fml(html,at(abschlussDachGenau(X))) -->
    ['the height of the Dachabschluss above the building is exactly '],[X],[' centimeters'].
pp_Fml(html,at(abschlussDachMin(X))) -->
    ['the height of the Dachabschluss above the building is at least '],[X],[' centimeters'].
pp_Fml(html,at(abschlussDachMax(X))) -->
    ['the height of the Dachabschluss above the building is at most '],[X],[' centimeters'].
% anteilDachbegruenung
pp_Fml(html,at(anteilDachbegruenungGenau(X))) -->
    ['the Anteil der Dachbegruenung is exactly '],[X],[' percent'].
pp_Fml(html,at(anteilDachbegruenung(X))) -->
    ['the Anteil der Dachbegruenung is at least '],[X],[' percent'].
pp_Fml(html,at(anteilDachbegruenungMax(X))) -->
    ['the Anteil der Dachbegruenung is at most '],[X],[' percent'].
% bbDachneigung
pp_Fml(html,at(bbDachneigungGenau(X))) -->
    ['the Dachneigung according to the textual regulation is  exactly '],[X],[' degrees'].
pp_Fml(html,at(bbDachneigungMin(X))) -->
    ['the Dachneigung according to the textual regulation is at least '],[X],[' degrees'].
pp_Fml(html,at(bbDachneigungMax(X))) -->
    ['the Dachneigung according to the textual regulation is at most '],[X],[' degrees'].
% dachflaeche
pp_Fml(html,at(dachflaecheGenau(X))) -->
    ['the Dachflaeche is exactly '],[X],[' square meters'].
pp_Fml(html,at(dachflaecheMin(X))) -->
    ['the Dachflaeche is at least '],[X],[' square meters'].
pp_Fml(html,at(dachflaecheMax(X))) -->
    ['the Dachflaeche is at most '],[X],[' square meters'].
% dachneigung
pp_Fml(html,at(dachneigungGenau(X))) -->
    ['the Dachneigung is exactly '],[X],[' degrees'].
pp_Fml(html,at(dachneigungMin(X))) -->
    ['the Dachneigung is at least '],[X],[' degrees'].
pp_Fml(html,at(dachneigungMax(X))) -->
    ['the Dachneigung is at most '],[X],[' degrees'].
% einfriedungHoeheGesamt
pp_Fml(html,at(einfriedungHoeheGesamtGenau(X))) -->
    ['the total height of the Einfriedung is exactly '],[X],[' centimeters'].
pp_Fml(html,at(einfriedungHoeheGesamtMin(X))) -->
    ['the total height of the Einfriedung is at least '],[X],[' centimeters'].
pp_Fml(html,at(einfriedungHoeheGesamt(X))) -->
    ['the total height of the Einfriedung is at most '],[X],[' centimeters'].
% einfriedungHoeheSockel
pp_Fml(html,at(einfriedungHoeheSockelGenau(X))) -->
    ['the height of the base of the Einfriedung is exactly '],[X],[' centimeters'].
pp_Fml(html,at(einfriedungHoeheSockelMin(X))) -->
    ['the height of the base of the Einfriedung is at least '],[X],[' centimeters'].
pp_Fml(html,at(einfriedungHoeheSockel(X))) -->
    ['the height of the base of the Einfriedung is at most '],[X],[' centimeters'].
% bauplatzUnterirdischeBebauung
pp_Fml(html,at(bauplatzUnterirdischeBebauungGenau(X))) -->
    ['the Anteil of the area taken by unterirdische Bauten/Bauteile is exactly '],[X],[' percent'].
pp_Fml(html,at(bauplatzUnterirdischeBebauungMin(X))) -->
    ['the Anteil of the area taken by unterirdische Bauten/Bauteile is at least '],[X],[' percent'].
pp_Fml(html,at(bauplatzUnterirdischeBebauungMax(X))) -->
    ['the Anteil of the area taken by unterirdische Bauten/Bauteile is at most '],[X],[' percent'].
% bbAusnuetzbarkeitFlaecheBGF
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheBGFGenau(X))) -->
    ['the area taken up by BGF is exactly '],[X],[' square meters'].
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheMin(X))) -->
    ['the area taken up by BGF is at least '],[X],[' square meters'].
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheBGF(X))) -->
    ['the area taken up by BGF is at most '],[X],[' square meters'].
% bbAusnuetzbarkeitFlaecheBGFRelativ
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheBGFRelativGenau(X))) -->
    ['the percentage of area taken up by BGF is exactly '],[X],[' percent'].
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheBGFRelativMin(X))) -->
    ['the percentage of area taken up by BGF is at least '],[X],[' percent'].
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheBGFRelativ(X))) -->
    ['the percentage of area taken up by BGF is at most '],[X],[' percent'].
% bbAusnuetzbarkeitFlaecheGrundflaechenbezug
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheGrundflaechenbezugGenau(X))) -->
    ['the ausgenutzte Grundflaeche is exactly '],[X],[' square meters'].
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheGrundflaechenbezugMin(X))) -->
    ['the ausgenutzte Grundflaeche is at least '],[X],[' square meters'].
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheGrundflaechenbezug(X))) -->
    ['the ausgenutzte Grundflaeche is at most '],[X],[' square meters'].
% bbAusnuetzbarkeitFlaecheGrundflaechenbezugRelativ
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheGrundflaechenbezugRelativGenau(X))) -->
    ['the percentage of ausgenutzte Grundflaeche is exactly '],[X],[' percent'].
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheGrundflaechenbezugRelativMin(X))) -->
    ['the percentage of ausgenutzte Grundflaeche is at least '],[X],[' percent'].
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheGrundflaechenbezugRelativ(X))) -->
    ['the percentage of ausgenutzte Grundflaeche is at most '],[X],[' percent'].
% bbAusnuetzbarkeitFlaecheNutzflaeche
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheNutzflaecheGenau(X))) -->
    ['the ausgenutzte Nutzflaeche is exactly '],[X],[' square meters'].
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheNutzflaecheMin(X))) -->
    ['the ausgenutzte Nutzflaeche is at least '],[X],[' square meters'].
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheNutzflaeche(X))) -->
    ['the ausgenutzte Nutzflaeche is at most '],[X],[' square meters'].
% bbAusnuetzbarkeitFlaecheNutzflaecheRelativ
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheBGFRelativGenau(X))) -->
    ['the percentage of ausgenutzte Nutzflaeche is exactly '],[X],[' percent'].
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheNutzflaecheRelativMin(X))) -->
    ['the percentage of ausgenutzte Nutzflaeche is at least '],[X],[' percent'].
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheNutzflaecheRelativ(X))) -->
    ['the percentage of ausgenutzte Nutzflaeche is at most '],[X],[' percent'].
% bbAusnuetzbarkeitFlaecheWohnnutzflaeche
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheWohnnutzflaecheGenau(X))) -->
    ['the ausgenutzte Wohnnutzflaeche is exactly '],[X],[' square meters'].
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheWohnnutzflaecheMin(X))) -->
    ['the ausgenutzte Wohnnutzflaeche is at least '],[X],[' square meters'].
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheWohnnutzflaeche(X))) -->
    ['the ausgenutzte Wohnnutzflaeche is at most '],[X],[' square meters'].
% bbAusnuetzbarkeitFlaecheWohnnutzflaecheRelativ
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheWohnnutzflaecheRelativGenau(X))) -->
    ['the percentage of area taken up by Wohnnutzflaeche is exactly '],[X],[' percent'].
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheWohnnutzflaecheRelativMin(X))) -->
    ['the percentage of area taken up by Wohnnutzflaeche is at least '],[X],[' percent'].
pp_Fml(html,at(bbAusnuetzbarkeitFlaecheWohnnutzflaecheRelativ(X))) -->
    ['the percentage of area taken up by Wohnnutzflaeche is at most '],[X],[' percent'].
% bbBebaubareFlaecheAbgegrenzt
pp_Fml(html,at(bbBebaubareFlaecheAbgegrenztGenau(X))) -->
    ['the bebaubare Flaeche des Bauplatzes is exactly '],[X],[' percent'].
pp_Fml(html,at(bbBebaubareFlaecheAbgegrenztMin(X))) -->
    ['the bebaubare Flaeche des Bauplatzes is at least '],[X],[' percent'].
pp_Fml(html,at(bbBebaubareFlaecheAbgegrenzt(X))) -->
    ['the bebaubare Flaeche des Bauplatzes is at most '],[X],[' percent'].
% bbBebaubareFlaecheGesamterBauplatz
pp_Fml(html,at(bbBebaubareFlaecheGesamterBauplatzGenau(X))) -->
    ['the percentage of bebaubare Flaeche on the whole Bauplatz is exactly '],[X],[' percent'].
pp_Fml(html,at(bbBebaubareFlaecheGesamterBauplatzMin(X))) -->
    ['the percentage of bebaubare Flaeche on the whole Bauplatz is at least '],[X],[' percent'].
pp_Fml(html,at(bbBebaubareFlaecheGesamterBauplatz(X))) -->
    ['the percentage of bebaubare Flaeche on the whole Bauplatz is at most '],[X],[' percent'].
% bbBebaubareFlaecheJeBauplatz
pp_Fml(html,at(bbBebaubareFlaecheJeBauplatzGenau(X))) -->
    ['the bebaubare Flaeche per Bauplatz is exactly '],[X],[' square meters'].
pp_Fml(html,at(bbBebaubareFlaecheJeBauplatzMin(X))) -->
    ['the bebaubare Flaeche per Bauplatz is at least '],[X],[' square meters'].
pp_Fml(html,at(bbBebaubareFlaecheJeBauplatzMax(X))) -->
    ['the bebaubare Flaeche per Bauplatz is at most '],[X],[' square meters'].
% bbBebaubareFlaecheJeGebaeude
pp_Fml(html,at(bbBebaubareFlaecheJeGebaeudeGenau(X))) -->
    ['the bebaubare Flaeche per Gebaeude is exactly '],[X],[' square meters'].
pp_Fml(html,at(bbBebaubareFlaecheJeGebaeudeMin(X))) -->
    ['the bebaubare Flaeche per Gebaeude is at least '],[X],[' square meters'].
pp_Fml(html,at(bbBebaubareFlaecheJeGebaeude(X))) -->
    ['the bebaubare Flaeche per Gebaeude is at most '],[X],[' square meters'].
% bbBebaubareFlaechefuerNebengebaeudeJeBauplatz
pp_Fml(html,at(bbBebaubareFlaechefuerNebengebaeudeJeBauplatzGenau(X))) -->
    ['the bebaubare Flaeche fuer Nebengebaeude je Bauplatz is exactly '],[X],[' square meters'].
pp_Fml(html,at(bbBebaubareFlaechefuerNebengebaeudeJeBauplatzMin(X))) -->
    ['the bebaubare Flaeche fuer Nebengebaeude je Bauplatz is at least '],[X],[' square meters'].
pp_Fml(html,at(bbBebaubareFlaechefuerNebengebaeudeJeBauplatzMax(X))) -->
    ['the bebaubare Flaeche fuer Nebengebaeude je Bauplatz is at most '],[X],[' square meters'].
% bbBebaubareFlaechefuerNebengebaeudeJeBaulos
pp_Fml(html,at(bbBebaubareFlaechefuerNebengebaeudeJeBaulosGenau(X))) -->
    ['the bebaubare Flaeche fuer Nebengebaeude je Baulos is exactly '],[X],[' square meters'].
pp_Fml(html,at(bbBebaubareFlaechefuerNebengebaeudeJeBaulosMin(X))) -->
    ['the bebaubare Flaeche fuer Nebengebaeude je Baulos is at least '],[X],[' square meters'].
pp_Fml(html,at(bbBebaubareFlaechefuerNebengebaeudeJeBaulosMax(X))) -->
    ['the bebaubare Flaeche fuer Nebengebaeude je Baulos is at most '],[X],[' square meters'].
% anzahlGeschosseOberirdisch
pp_Fml(html,at(anzahlGeschosseOberirdischGenau(X))) -->
    ['the number of floors in the whole building is exactly '],[X].
pp_Fml(html,at(anzahlGeschosseOberirdischMin(X))) -->
    ['the number of floors in the whole building is at least '],[X].
pp_Fml(html,at(maxAnzahlGeschosseOberirdisch(X))) -->
    ['the number of floors in the whole building is at most '],[X].
% anzahlGeschosseOberirdischDachgeschoss
pp_Fml(html,at(anzahlGeschosseOberirdischDachgeschossGenau(X))) -->
    ['the number of Dachgeschosse is exactly '],[X].
pp_Fml(html,at(anzahlGeschosseOberirdischDachgeschossMin(X))) -->
    ['the number of Dachgeschosse is at least '],[X].
pp_Fml(html,at(maxAnzahlGeschosseOberirdischDachgeschoss(X))) -->
    ['the number of Dachgeschosse is at most '],[X].
% anzahlGeschosseOberirdischOhneDachgeschoss
pp_Fml(html,at(anzahlGeschosseOberirdischOhneDachgeschossGenau(X))) -->
    ['the number of floors without Dachgeschosse is exactly '],[X].
pp_Fml(html,at(anzahlGeschosseOberirdischOhneDachgeschossMin(X))) -->
    ['the number of floors without Dachgeschosse is at least '],[X].
pp_Fml(html,at(maxAnzahlGeschosseOberirdischOhneDachgeschoss(X))) -->
    ['the number of floors without Dachgeschosse is at most '],[X].
% geschossanzahl
pp_Fml(html,at(geschossanzahlGenau(X))) -->
    ['the number of floors for an Einkaufszentrum is exactly '],[X].
pp_Fml(html,at(geschossanzahlMin(X))) -->
    ['the number of floors for an Einkaufszentrum is at least '],[X].
pp_Fml(html,at(zulaessigeGeschossanzahl(X))) -->
    ['the number of floors for an Einkaufszentrum is at most '],[X].
% einkaufszentrumFlaeche
pp_Fml(html,at(einkaufszentrumFlaecheGenau(X))) -->
    ['the area of the Einkaufszentrum per Widmungsflaeche is exactly '],[X],[' square meters'].
pp_Fml(html,at(einkaufszentrumFlaecheMin(X))) -->
    ['the area of the Einkaufszentrum per Widmungsflaeche is at least '],[X],[' square meters'].
pp_Fml(html,at(einkaufszentrumMaxFlaeche(X))) -->
    ['the area of the Einkaufszentrum per Widmungsflaeche is at most '],[X],[' square meters'].
% grossbauvorhabenFlaeche
pp_Fml(html,at(grossbauvorhabenFlaecheGenau(X))) -->
    ['the area of the Grossbauvorhaben per Widmungsflaeche is exactly '],[X],[' square meters'].
pp_Fml(html,at(grossbauvorhabenFlaecheMin(X))) -->
    ['the area of the Grossbauvorhaben per Widmungsflaeche is at least '],[X],[' square meters'].
pp_Fml(html,at(grossbauvorhabenMaxFlaeche(X))) -->
    ['the area of the Grossbauvorhaben per Widmungsflaeche is at most '],[X],[' square meters'].
% anschlussGebaeudeAnGelaende
pp_Fml(html,at(anschlussGebaeudeAnGelaende(X))) -->
    ['the Hoehenlage der Grundflaeche is exactly '],[X],[' centimeters'].
pp_Fml(html,at(anschlussGebaeudeAnGelaendeMin(X))) -->
    ['the Hoehenlage der Grundflaeche is at least '],[X],[' centimeters'].
pp_Fml(html,at(anschlussGebaeudeAnGelaendeMax(X))) -->
    ['the Hoehenlage der Grundflaeche is at most '],[X],[' centimeters'].
% bbBauklasse
pp_Fml(html,at(bbBauklasseGenau(X))) -->
    ['the height of the building of Bauklasse vi is exactly '],[X],[' centimeters'].
pp_Fml(html,at(bbBauklasseMinimum(X))) -->
    ['the height of the building of Bauklasse vi is at least '],[X],[' centimeters'].
pp_Fml(html,at(bbBauklasseMaximum(X))) -->
    ['the height of the building of Bauklasse vi is at most '],[X],[' centimeters'].
% fbokWohnungen
pp_Fml(html,at(fbokWohnungenGenau(X))) -->
    ['the height of the Fussbodenoberkante of the first Wohnungsgeschoss is exactly '],[X],[' centimeters'].
pp_Fml(html,at(fbokMinimumWohnungen(X))) -->
    ['the height of the Fussbodenoberkante of the first Wohnungsgeschoss is at least '],[X],[' centimeters'].
pp_Fml(html,at(fbokWohnungenMax(X))) -->
    ['the height of the Fussbodenoberkante of the first Wohnungsgeschoss is at most '],[X],[' centimeters'].
% gebaeudeHoehe
pp_Fml(html,at(gebaeudeHoeheGenau(X))) -->
    ['the height of the building is exactly '],[X],[' centimeters'].
pp_Fml(html,at(gebaeudeHoeheMin(X))) -->
    ['the height of the building is at least '],[X],[' centimeters'].
pp_Fml(html,at(gebaeudeHoeheMax(X))) -->
    ['the height of the building is at most '],[X],[' centimeters'].
% hoeheWohngebaeude
pp_Fml(html,at(hoeheWohngebaeudeGenau(X))) -->
    ['the height of the Kleinhaus is exactly '],[X],[' centimeters'].
pp_Fml(html,at(hoeheWohngebaeudeMin(X))) -->
    ['the height of the Kleinhaus is at least '],[X],[' centimeters'].
pp_Fml(html,at(maxHoeheWohngebaeude(X))) -->
    ['the height of the Kleinhaus is at most '],[X],[' centimeters'].
% raumhoeheEG
pp_Fml(html,at(raumhoeheEGGenau(X))) -->
    ['the height of the rooms on the ground floor in the Geschaeftsviertel is exactly '],[X],[' centimeters'].
pp_Fml(html,at(mindestraumhoeheEG(X))) -->
    ['the height of the rooms on the ground floor in the Geschaeftsviertel is at least '],[X],[' centimeters'].
pp_Fml(html,at(raumhoeheEGMax(X))) -->
    ['the height of the rooms on the ground floor in the Geschaeftsviertel is at most '],[X],[' centimeters'].
% gelaendeneigung
pp_Fml(html,at(gelaendeneigungGenau(X))) -->
    ['the Gelaendeneigung is exactly '],[X],[' degrees'].
pp_Fml(html,at(gelaendeneigungMin(X))) -->
    ['the Gelaendeneigung is at least '],[X],[' degrees'].
pp_Fml(html,at(gelaendeneigungMax(X))) -->
    ['the Gelaendeneigung is at most '],[X],[' degrees'].
% arkadeHoehe
pp_Fml(html,at(arkadeHoeheGenau(X))) -->
    ['the height of the Arkade is exactly '],[X],[' centimeters'].
pp_Fml(html,at(arkadeHoehe(X))) -->
    ['the height of the Arkade is at least '],[X],[' centimeters'].
pp_Fml(html,at(arkadeHoeheMax(X))) -->
    ['the height of the Arkade is at most '],[X],[' centimeters'].
% arkadeLaenge
pp_Fml(html,at(arkadeLaengeGenau(X))) -->
    ['the length of the Arkade is exactly '],[X],[' centimeters'].
pp_Fml(html,at(arkadeLaenge(X))) -->
    ['the length of the Arkade is at least '],[X],[' centimeters'].
pp_Fml(html,at(arkadeLaengeMax(X))) -->
    ['the length of the Arkade is at most '],[X],[' centimeters'].
% durchfahrtBreite
pp_Fml(html,at(durchfahrtBreiteGenau(X))) -->
    ['the width of the Durchfahrt is exactly '],[X],[' centimeters'].
pp_Fml(html,at(durchfahrtBreite(X))) -->
    ['the width of the Durchfahrt is at least '],[X],[' centimeters'].
pp_Fml(html,at(durchfahrtBreiteMax(X))) -->
    ['the width of the Durchfahrt is at most '],[X],[' centimeters'].
% durchfahrtHoehe
pp_Fml(html,at(durchfahrtHoeheGenau(X))) -->
    ['the height of the Durchfahrt is exactly '],[X],[' centimeters'].
pp_Fml(html,at(durchfahrtHoehe(X))) -->
    ['the height of the Durchfahrt is at least '],[X],[' centimeters'].
pp_Fml(html,at(durchfahrtHoeheMax(X))) -->
    ['the height of the Durchfahrt is at most '],[X],[' centimeters'].
% durchgangBreite
pp_Fml(html,at(durchgangBreiteGenau(X))) -->
    ['the width of the Durchgang is exactly '],[X],[' centimeters'].
pp_Fml(html,at(durchgangBreite(X))) -->
    ['the width of the Durchgang is at least '],[X],[' centimeters'].
pp_Fml(html,at(durchgangBreiteMax(X))) -->
    ['the width of the Durchgang is at most '],[X],[' centimeters'].
% durchgangHoehe
pp_Fml(html,at(durchgangHoeheGenau(X))) -->
    ['the height of the Durchgang is exactly '],[X],[' centimeters'].
pp_Fml(html,at(durchgangHoehe(X))) -->
    ['the height of the Durchgang is at least '],[X],[' centimeters'].
pp_Fml(html,at(durchgangHoeheMax(X))) -->
    ['the height of the Durchgang is at most '],[X],[' centimeters'].
% laubengangHoehe
pp_Fml(html,at(laubengangHoeheGenau(X))) -->
    ['the height of the Laubengang is exactly '],[X],[' centimeters'].
pp_Fml(html,at(laubengangHoehe(X))) -->
    ['the height of the Laubengang is at least '],[X],[' centimeters'].
pp_Fml(html,at(laubengangHoeheMax(X))) -->
    ['the height of the Laubengang is at most '],[X],[' centimeters'].
% laubengangLaenge
pp_Fml(html,at(laubengangLaengeGenau(X))) -->
    ['the length of the Laubengang is exactly '],[X],[' centimeters'].
pp_Fml(html,at(laubengangLaenge(X))) -->
    ['the length of the Laubengang is at least '],[X],[' centimeters'].
pp_Fml(html,at(laubengangLaengeMax(X))) -->
    ['the length of the Laubengang is at most '],[X],[' centimeters'].
% bbAusnuetzbarkeitWidmungskategorieGefoerderterWohnbau
pp_Fml(html,at(bbAusnuetzbarkeitWidmungskategorieGefoerderterWohnbauGenau(X))) -->
    ['the Ausnuetzbarkeit der Widmungskategorie Gef&ouml;rderter Wohnbau is exactly '],[X],[' percent'].
pp_Fml(html,at(bbAusnuetzbarkeitWidmungskategorieGefoerderterWohnbau(X))) -->
    ['the Ausnuetzbarkeit der Widmungskategorie Gef&ouml;rderter Wohnbau is at least '],[X],[' percent'].
pp_Fml(html,at(bbAusnuetzbarkeitWidmungskategorieGefoerderterWohnbauMax(X))) -->
    ['the Ausnuetzbarkeit der Widmungskategorie Gef&ouml;rderter Wohnbau is at most '],[X],[' percent'].
% stellplatz
pp_Fml(html,at(stellplatzGenau(X))) -->
    ['the number of Stellpl&auml;tze f&uuml;r Grossbauvorhaben is exactly '],[X].
pp_Fml(html,at(stellplatzMin(X))) -->
    ['the number of Stellpl&auml;tze f&uuml;r Grossbauvorhaben is at least '],[X].
pp_Fml(html,at(stellplatzMax(X))) -->
    ['the number of Stellpl&auml;tze f&uuml;r Grossbauvorhaben is at most '],[X].
% stellplatzregulativUmfangAbsolut
pp_Fml(html,at(stellplatzregulativUmfangAbsolutGenau(X))) -->
    ['the Stellplatzregulativ is exactly '],[X].
pp_Fml(html,at(stellplatzregulativUmfangAbsolutMin(X))) -->
    ['the Stellplatzregulativ is at least '],[X].
pp_Fml(html,at(stellplatzregulativUmfangMaximumAbsolut(X))) -->
    ['the Stellplatzregulativ is at most '],[X].
% stellplatzregulativUmfangRelativ
pp_Fml(html,at(stellplatzregulativUmfangRelativGenau(X))) -->
    ['the deviation from the Stellplatzregulativ is exactly '],[X],[' percent'].
pp_Fml(html,at(stellplatzregulativUmfangMinimumRelativ(X))) -->
    ['the deviation from the Stellplatzregulativ is at least '],[X],[' percent'].
pp_Fml(html,at(stellplatzregulativUmfangMaximumRelativ(X))) -->
    ['the deviation from the Stellplatzregulativ is at most '],[X],['percent'].
% gehsteigbreite
pp_Fml(html,at(gehsteigbreiteGenau(X))) -->
    ['the Gehsteigbreite is exactly '],[X],[' centimeters'].
pp_Fml(html,at(gehsteigbreiteMin(X))) -->
    ['the Gehsteigbreite is at least '],[X],[' centimeters'].
pp_Fml(html,at(gehsteigbreiteMax(X))) -->
    ['the Gehsteigbreite is at most '],[X],[' centimeters'].
% strassenbreite
pp_Fml(html,at(strassenbreiteGenau(X))) -->
    ['the Strassenbreite is exactly '],[X],[' centimeters'].
pp_Fml(html,at(strassenbreiteMin(X))) -->
    ['the Strassenbreite is at least '],[X],[' centimeters'].
pp_Fml(html,at(strassenbreiteMax(X))) -->
    ['the Strassenbreite is at most '],[X],[' centimeters'].
% bbAusnuetzbarkeitVolumenBaumasse
pp_Fml(html,at(bbAusnuetzbarkeitVolumenBaumasseGenau(X))) -->
    ['the volumenbezogene Ausn&uuml;tzbarkeit bezogen auf die Baumasse is exactly '],[X],[' cubic meters'].
pp_Fml(html,at(bbAusnuetzbarkeitVolumenBaumasseMin(X))) -->
    ['the volumenbezogene Ausn&uuml;tzbarkeit bezogen auf die Baumasse is at least '],[X],[' cubic meters'].
pp_Fml(html,at(bbAusnuetzbarkeitVolumenBaumasse(X))) -->
    ['the volumenbezogene Ausn&uuml;tzbarkeit bezogen auf die Baumasse is at most '],[X],[' cubic meters'].
% bbAusnuetzbarkeitVolumenBaumasseRelativ
pp_Fml(html,at(bbAusnuetzbarkeitVolumenBaumasseRelativGenau(X))) -->
    ['the volumenbezogene Ausn&uuml;tzbarkeit bezogen auf die Baumasse is exactly '],[X],[' percent'].
pp_Fml(html,at(bbAusnuetzbarkeitVolumenBaumasseRelativMin(X))) -->
    ['the volumenbezogene Ausn&uuml;tzbarkeit bezogen auf die Baumasse is at least '],[X],[' percent'].
pp_Fml(html,at(bbAusnuetzbarkeitVolumenBaumasseRelativ(X))) -->
    ['the volumenbezogene Ausn&uuml;tzbarkeit bezogen auf die Baumasse is at most '],[X],[' percent'].
% bbAusnuetzbarkeitVolumenRelativ
pp_Fml(html,at(bbAusnuetzbarkeitVolumenRelativGenau(X))) -->
    ['the volumenbezogene Ausn&uuml;tzbarkeit bezogen auf die Grundfl&auml;che is exactly '],[X],[' percent'].
pp_Fml(html,at(bbAusnuetzbarkeitVolumenRelativMin(X))) -->
    ['the volumenbezogene Ausn&uuml;tzbarkeit bezogen auf die Grundfl&auml;che is at least '],[X],[' percent'].
pp_Fml(html,at(bbAusnuetzbarkeitVolumenRelativ(X))) -->
    ['the volumenbezogene Ausn&uuml;tzbarkeit bezogen auf die Grundfl&auml;che is at most '],[X],[' percent'].
% umbaubarerRaumBauplatz
pp_Fml(html,at(umbaubarerRaumBauplatzGenau(X))) -->
    ['the umbaubarer Raum on the Bauplatz is exactly '],[X],[' cubic meters'].
pp_Fml(html,at(umbaubarerRaumBauplatzMin(X))) -->
    ['the umbaubarer Raum on the Bauplatz is at least '],[X],[' cubic meters'].
pp_Fml(html,at(umbaubarerRaumBauplatzMax(X))) -->
    ['the umbaubarer Raum on the Bauplatz is at most '],[X],[' cubic meters'].
% umbaubarerRaumGebaeude
pp_Fml(html,at(umbaubarerRaumGebaeudeGenau(X))) -->
    ['the umbaubarer Raum for Geba&auml;ude is exactly '],[X],[' square meters'].
pp_Fml(html,at(umbaubarerRaumGebaeudeMin(X))) -->
    ['the umbaubarer Raum for Geba&auml;ude is at least '],[X],[' square meters'].
pp_Fml(html,at(umbaubarerRaumGebaeudeMax(X))) -->
    ['the umbaubarer Raum for Geba&auml;ude is at most '],[X],[' square meters'].
% umbaubarerRaumGebaeudeteil
pp_Fml(html,at(umbaubarerRaumGebaeudeteilGenau(X))) -->
    ['the umbaubarer Raum for Geba&auml;udeteile is exactly '],[X],[' square meters'].
pp_Fml(html,at(umbaubarerRaumGebaeudeteilMin(X))) -->
    ['the umbaubarer Raum for Geba&auml;udeteile is at least '],[X],[' square meters'].
pp_Fml(html,at(umbaubarerRaumGebaeudeteilMax(X))) -->
    ['the umbaubarer Raum for Geba&auml;udeteile is at most '],[X],[' square meters'].
% vorstehendeBauelementeAusladung
pp_Fml(html,at(vorstehendeBauelementeAusladungGenau(X))) -->
    ['the maximale Ausladung vorstehender Bauteile is exactly '],[X],[' centimeters'].
pp_Fml(html,at(vorstehendeBauelementeAusladungMin(X))) -->
    ['the maximale Ausladung vorstehender Bauteile is at least '],[X],[' centimeters'].
pp_Fml(html,at(vorstehendeBauelementeAusladungMax(X))) -->
    ['the maximale Ausladung vorstehender Bauteile is at most '],[X],[' centimeters'].
%
/*  other atomics with arguments:
*/
pp_Fml(html,at(X)) --> {\+ atom(X), term_to_atom(X,Y)}, [Y].
pp_Fml(html,at(X)) --> {atom(X)}, [X].
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
pp_Fml(html,neg(modal(for,A,B))) -->
    ['it is not forbidden that ('],
    pp_Fml(html,A),
    ['), given ('],pp_Fml(html,B),[')'].
pp_Fml(html,neg(modal(obl,A,B))) -->
    ['it is not obligatory that ('],
    pp_Fml(html,A),
    ['), given ('],pp_Fml(html,B),[')'].
pp_Fml(html,neg(modal(rec,A,B))) -->
    ['it is not recommended that ('],
    pp_Fml(html,A),
    ['), given ('],pp_Fml(html,B),[')'].
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
% NOTE: might not need the for BRISEprover.
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


/* pp_Fml_list//2
   DCG to pretty print a list of formulae.
*/
pp_Fml_list(_, []) --> [].
pp_Fml_list(Form, [inv(_)|Tail]) -->
    pp_Fml_list(Form,Tail).
pp_Fml_list(Form, [A|[]]) --> 
    pp_Fml(Form, A).
pp_Fml_list(Form, [A|Tail]) --> 
    pp_Fml(Form, A), [', '], pp_Fml_list(Form, Tail).


/* pp_Fml_list//3
   DCG for writing a list of formulae in html format, depending on the
   side of the sequent.
   The additional argument specifies the left or right hand side of
   the sequent.
*/
pp_Fml_list(html,l, []) --> ['true'].
pp_Fml_list(html,r, []) --> ['we have a contradiction'].
pp_Fml_list(html,Side,[inv(_)|Tail]) --> pp_Fml_list(html,Side,Tail).
pp_Fml_list(html,_, [A|[]]) --> 
    pp_Fml(html, A).
pp_Fml_list(html,l, [A|Tail]) --> 
    pp_Fml(html, A), [' and '], pp_Fml_list(html,l, Tail).
pp_Fml_list(html,r, [A|Tail]) --> 
    pp_Fml(html, A), [' or '], pp_Fml_list(html,r, Tail).


/* pp_Seq//2
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


/* pp_Seq_arrow//1
   DCG for printing the sequent arrow in the different formats
*/
pp_Seq_arrow(screen) --> [' => '].
pp_Seq_arrow(latex) --> ['\\seq'].
pp_Seq_arrow(html) --> [' => '].


/* pp_Seq_list//2
   DCG for printing a list of sequents
   NOTE: might be superfluous now (came from linear nested sequents)
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
pp_Seq_list(html,[]) --> [].
pp_Seq_list(html,[Seq|[]]) -->
    ['( '], pp_Seq(html,Seq), [' )'].
pp_Seq_list(html, [Seq1,Seq2|Tail]) -->
    ['( '], pp_Seq(html,Seq1), [' ); '], pp_Seq_list(html, [Seq2|Tail]).


/* pp_assumptions//2
   DCG for pretty printing the assumptions
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
    

/* pp_derivation//3
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
pp_derivation(screen,N,node(measurefact, PF, Seq, _)) -->
    pp_nl_tab(N),
    ['measurefact['], pp_Seq(screen,PF), [']( '],
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
pp_derivation(latex,N,node(measurefact, PF, Seq, _)) -->
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
    ['\\\\ \\text{is not overruled:}\\end{array}'], 
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
    pp_Fml(latex,Fml1),
    pp_nl_tab(N),
    ['\\text{ because:} \\end{array}'],
    pp_nl_tab(N),['&'],
    pp_derivation_list(latex,N+2,[T1,T2,T3]).

% clauses for html:
pp_derivation(html,N,nonderivable) -->
    pp_nl_tab(N),['input is not derivable'].
pp_derivation(html,N,node(Name,init, _, Seq, _)) -->
    pp_html_derivable_statement(Name,Seq),
    pp_nl_tab(N),
    ["Because if we assume a formula, then it is assumed to be the case.
    </div>"].
pp_derivation(html,N,node(Name,botL, _, Seq, _)) -->
    pp_html_derivable_statement(Name,Seq),
    pp_nl_tab(N),
    ["Because everything follows if we assume <code>false</code>."],
    pp_nl_tab(N),
    ["</div>"].
pp_derivation(html,N,node(Name,topR, _, Seq, _)) -->
    pp_html_derivable_statement(Name,Seq),
    pp_nl_tab(N),
    ["Because <code>true</code> is always true.
    </div>"].
pp_derivation(html,N,node(Name,fact, PF, Seq, _)) -->
    pp_html_derivable_statement(Name,Seq),
    pp_nl_tab(N),
    ["Because this follows immediately from the assumed fact <br />"],
    pp_nl_tab(N),
    ["<code>"],
    pp_Seq(html,PF),
    pp_nl_tab(N),
    ["</code>."],pp_nl_tab(N),["</div>"].
pp_derivation(html,N,node(Name,measurefact, PF, Seq, _)) -->
    pp_html_derivable_statement(Name,Seq),
    pp_nl_tab(N),
    ["Because this follows immediately from the following fact about the measures involved: <br />"],
    pp_nl_tab(N),
    ["<code>"],
    pp_Seq(html,PF),
    pp_nl_tab(N),
    ["</code>."],pp_nl_tab(N),["</div>"].
% propositional rules:
pp_derivation(html,N,node(Name,Rule,PF,Seq,Suc)) -->
    {rule_type(Rule,propositional)},
    pp_nl_tab(N),
    pp_html_derivable_statement(Name,Seq),
    ["Because it follows by propositional logic from the following:"],
    pp_nl_tab(N),
    ["<ul>"],
    pp_html_skip_list_new(N + 2,Suc),
    pp_nl_tab(N),
    ["</ul>"],
    pp_nl_tab(N),
    ["<button class=\"button\" onclick=\"myFunction('fct"],
    format_name(Name),["_trunc')\">Why does it follow from the above?</button>"],
    pp_nl_tab(N),
    pp_nl_tab(N),
    ["<div class=\"abstract\" id=\"fct"],format_name(Name),["_trunc\">"],
    ['The detailed explanation of the statement above is the following:<br />'],
    pp_html_truncated_new(N + 2,node(Name, Rule, PF, Seq, Suc)),
    pp_nl_tab(N),
    ["</div>"],
    pp_nl_tab(N),
    ["</div>"].
% Monotonicity rule:
pp_derivation(html,N,node(Name,mon(Op1,Op2),PF,Seq,[Suc1,Suc2,Suc3])) -->
    pp_html_derivable_statement(Name,Seq),
    pp_nl_tab(N),
    ["Because it follows immediately from <br /><code>"],
    pp_nl_tab(N),pp_Seq(html,PF),["</code>.<br />"],
    pp_nl_tab(N),
    ["That statement holds due to <i>monotonicity of the
operators</i> <code>"], pp_Op(html,Op1),["</code> and <code>"], pp_Op(html,Op2), ["</code>, i.e.,
if something is obligatory, then everything which follows is also
obligatory under logically equivalent conditions.<br />"],
    pp_nl_tab(N),
    ["In particular, we have the implication of the obligatory statements:<br />"],
    pp_html_successors_new(N+2,[Suc1]),
    pp_nl_tab(N),["And, the conditions are equivalent:"],
    pp_html_successors_new(N+2,[Suc2,Suc3]),
    pp_nl_tab(N),
    ["</div>"].
% P rule:
pp_derivation(html,N,node(Name,pRule(Op),PF,Seq,Suc)) -->
    pp_html_derivable_statement(Name,Seq),
    pp_nl_tab(N),
    ["Because it follows immediately from <br /><code>"],
    pp_nl_tab(N),pp_Seq(html,PF),["</code>.<br />"],
    pp_nl_tab(N),
    ["That statement holds since the operator <code>"],
    pp_Op(html,Op),
    ["</code> is <i>nontrivial</i>, i.e., nothing which is logically impossible
is obligatory, and nothing which is logically always true is forbidden.<br />"],
    pp_nl_tab(N),
    ["In particular, the obligatory statement is logically impossible,
resp. the forbidden statement is always true:<br />"],
    pp_html_successors_new(N+2,Suc),
    pp_nl_tab(N),
    ["</div>"].
% conflict/D rule:
pp_derivation(html,N,node(Name,confl(Op1,Op2),PF,Seq,[Suc1,Suc2,Suc3])) -->
    pp_html_derivable_statement(Name,Seq),
    pp_nl_tab(N),
    ["Because it follows immediately from <br /><code>"],
    pp_nl_tab(N),pp_Seq(html,PF),["</code>.<br />"],
    pp_nl_tab(N),
    ["That statement holds since we have <i>no conflicts</i> between
operators <code>"],
    pp_Op(html,Op1),["</code> and <code>"],pp_Op(html,Op2),
    ["</code>, i.e.,
it is not the case that two conflicting statements are obligatory under logically equivalent conditions.<br />"],
    pp_nl_tab(N),
    ["In particular, the two statements are in conflict:<br />"],
    pp_html_successors_new(N+2,[Suc1]),
    pp_nl_tab(N),["And, the conditions are equivalent:"],
    pp_html_successors_new(N+2,[Suc2,Suc3]),
    pp_nl_tab(N),
    ["</div>"].
% assumption right rule:
pp_derivation(html,N,node(Name,asmpR(Op1,Assumption), PF, Seq, [T1,T2|Suc])) -->
    pp_html_derivable_statement(Name,Seq),
    ["Because it follows from immediately from<br /> <code>"],pp_nl_tab(N),
    pp_Seq(html,PF),["</code>.<br />"],pp_nl_tab(N),
    ["That statement is derivable from the deontic assumption <code>"],
    pp_Fml(html,Assumption),
    ["</code> and monotonicity of the operator <code>"],pp_Op(html,Op1),["</code>:<br />
    "],
    pp_nl_tab(N),
    ["<ul>"],pp_nl_tab(N+2),
    ["<li> The assumption is applicable because:<br />"],pp_nl_tab(N+2),
    pp_derivation(html,N+2,T1),
    ["</li>"],pp_nl_tab(N+2),
    ["<li> The condition is implied because:<br />"],pp_nl_tab(N+2),
    pp_derivation(html,N+2,T2),
    ["</li>"],pp_nl_tab(N+2),
    pp_html_aux_list_new(N + 2,Suc),
    pp_nl_tab(N),
    ["</ul>"], pp_nl_tab(N),
%    pp_html_successors_new(N,Suc),
%    pp_nl_tab(N),
    ["</div>"].
% assumption left rule:
pp_derivation(html,N,node(Name,asmpL(Op1,Assumption), PF, Seq, [T1,T2|Suc])) -->
    pp_nl_tab(N),
    pp_html_derivable_statement(Name,Seq),
    ["Because it follows from immediately from<br/> <code>"],pp_nl_tab(N),
    pp_Seq(html,PF),["</code>.<br />"],pp_nl_tab(N),
    ["That statement is derivable from the deontic assumption <code>"],
/*
    ["Because it follows from the following using the deontic
assumption <code>"],
*/
    pp_Fml(html,Assumption),
    ["</code> and the axiom that there are no conflicts between that
operator and <code>"], pp_Op(html,Op1),["</code>. In particular:<br />
    "],
    pp_nl_tab(N),
    ["<ul>"],pp_nl_tab(N+2),
    ["<li> The assumption is applicable because:<br />"],pp_nl_tab(N+2),
    pp_derivation(html,N+2,T1),
    ["</li>"],pp_nl_tab(N+2),
    ["<li> The conditions are in conflict because:<br />"],pp_nl_tab(N+2),
    pp_derivation(html,N+2,T2),
    ["</li>"],pp_nl_tab(N+2),
    pp_html_aux_list_new(N + 2,Suc),
    pp_nl_tab(N),
    ["</ul>"], pp_nl_tab(N),
%    pp_html_successors_new(N,Suc),
%    pp_nl_tab(N),
    ["</div>"].
% clauses for the different blocks in the assumption rules:    
pp_derivation(html,N,node(_,no_p_conflict(Op,Seq))) -->
    pp_nl_tab(N),
    ['The operator <code>'],pp_Op(html,Op)
    ,['</code> is nontrivial, but there is no conflict, because we cannot
derive that'], pp_Seq(html,Seq),
    pp_nl_tab(N),
    ["</div>"].
pp_derivation(html,_,node(_,no_p_conflict(na))) -->
    [].
pp_derivation(html,N,node(_,not_overruled(Assumption),[]))
-->
    pp_nl_tab(N),
    ["The deontic assumption <code>"],pp_Fml(html,Assumption),
    ["</code> is not overruled by any conflicting more specific assumption, because there are no conflicting (and not inferior) assumptions."].
pp_derivation(html,N,node(_,not_overruled(Assumption),[S|Suc]))
-->
    pp_nl_tab(N),
    ["The deontic assumption <code>"],pp_Fml(html,Assumption),
    ["</code> is not overruled by any conflicting assumption because of the following: "],
    pp_html_successors_new(N + 2, [S|Suc]).
pp_derivation(html,N,node(_,notapplicable(Fml,Seq))) -->
    pp_nl_tab(N),
    ["The deontic assumption <br />"],
    pp_nl_tab(N),["<code>"],
    pp_Fml(html, Fml),["</code>"],pp_nl_tab(N),
    [" is not applicable because we cannot derive <br />"],
    pp_nl_tab(N),["<code>"],
    pp_Seq(html,Seq),["</code>"].
pp_derivation(html,N,node(_,noconflict(Fml,Seq))) -->
    pp_nl_tab(N),
    ["The deontic assumption <br />"],
    pp_nl_tab(N),["<code>"],
    pp_Fml(html, Fml),["</code>"],pp_nl_tab(N),
    [" is not in conflict because we cannot derive <br />"],
    pp_nl_tab(N),["<code>"],
    pp_Seq(html,Seq),["</code>"].

% TODO: CHECK WHAT THIS DOES!
pp_derivation(html,N,node(notimplied(Fml,Seq))) -->
    pp_nl_tab(N),
    ['\\begin{array}[b]{l}\\text{For }'],
    pp_Fml(html,Fml), [':\\\\ '],
    ['\\text{no conflict, because}\\\\ \\nentails'],
    pp_Seq(html,Seq),['\\end{array}'].

pp_derivation(html,N,node(_,superior(Norm1:Fml1, Norm2:Fml2))) -->
    pp_nl_tab(N),
    ["The deontic assumption <br />"],
    pp_nl_tab(N),["<code>"],
    pp_Fml(html, Norm2:Fml2),["</code>"],pp_nl_tab(N),
    [" is beaten by the assumption <br />"],
    pp_nl_tab(N),["<code>"],
    pp_Fml(html,Norm1:Fml1),["</code>, because <code>"],
    pp_norm(html,Norm1),["</code> beats <code>"],pp_norm(html,Norm2),["</code> ."].
pp_derivation(html,N,node(_,notoverruled(Fml,Seq,[Suc]))) -->
    pp_nl_tab(N),
    ["The deontic assumption <br />"],
    pp_nl_tab(N),["<code>"],
    pp_Fml(html,Fml),["</code>"],pp_nl_tab(N),
    [" is not overruling, because:"],pp_nl_tab(N),
    ["<ul>"],pp_nl_tab(N+2),
    ["<li> it is not more specific since we cannot derive <code>"],
    pp_Seq(html,Seq),["</code>, and also"],pp_nl_tab(N+2),
    ["<li>"],
    pp_derivation(html,N+2,Suc),
    ["</li>"],pp_nl_tab(N),
    ["</ul>"].
%    pp_html_successors_new(N + 2, [Suc]).
/*
pp_derivation(html,N,node(Name,notoverruled(Fml,Seq,[Suc]))) -->
    pp_nl_tab(N),
    ["The deontic assumption <br />"],
    pp_nl_tab(N),["<code>"],
    pp_Fml(html,Fml),["</code>"],pp_nl_tab(N),
    [" is not overruling, because it is not more specific since we cannot derive <code>"],
    pp_Seq(html,Seq),["</code>, and also because of the following:"],
    pp_derivation(html,N+2,Suc).
*/
%    pp_html_successors_new(N + 2, [Suc]).
pp_derivation(html,N,node(_,overrides(Fml1, Fml2),[T1,T2,T3])) -->
    pp_nl_tab(N),
/*
    ["The deontic assumption <br />"],
    pp_nl_tab(N),["<code>"],
    pp_Fml(html, Fml2),["</code>"],pp_nl_tab(N),
    [" is overruled by the more specific deontic assumption <br />"],
*/
    ["It is overruled by the more specific deontic assumption <br />"],
    pp_nl_tab(N),["<code>"],
    pp_Fml(html, Fml1),["</code>"],pp_nl_tab(N),
    [" because of the following:"],pp_nl_tab(N),
    ["<ul>"],pp_nl_tab(N+2),
    ["<li> The assumption is applicable because:<br /> "], pp_nl_tab(N+2),
    pp_derivation(html,N+2,T1), pp_nl_tab(N+2),
    ["</li>"],pp_nl_tab(N+2),
    ["<li> The assumption is more specific because:<br /> "],pp_derivation(html,N+2,T2), pp_nl_tab(N+2),
    ["</li>"],pp_nl_tab(N+2),
    ["<li> The assumption <code>"], pp_Fml(html,Fml1),
    ["</code> is in conflict with the assumption <code>"],
    pp_Fml(html,Fml2), ["</code> because:<br /> "],
    pp_derivation(html,N+2,T3),pp_nl_tab(N+2),
    ["</li>"],pp_nl_tab(N),
    ["</ul>"].
%    pp_html_successors_new(N + 2, [T1,T2,T3]).
/*
pp_derivation(html,N,node(Name,overrides(Fml1, Fml2),[T1,T2,T3])) -->
    pp_nl_tab(N),
    ["The deontic assumption <br />"],
    pp_nl_tab(N),["<code>"],
    pp_Fml(html, Fml2),["</code>"],pp_nl_tab(N),
    [" is overuled by the more specific deontic assumption <br />"],
    pp_nl_tab(N),["<code>"],
    pp_Fml(html, Fml1),["</code>"],pp_nl_tab(N),
    [" because of the following:"],
    pp_html_successors_new(N + 2, [T1,T2,T3]).
*/
pp_derivation(html, _,_) --> [].


/* pp_derivation_list//3
   DCG for pretty printing a list of derivations
*/
% TODO: [ ] add this for html.
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
pp_derivation_list(html,_,_) --> ['pp_derivation_list(html,_)'].


/* format_name
   DCG for printing the name of a node in html
*/
format_name([]) --> [].
format_name([Number|Tail]) --> format_name(Tail), ['_',Number].


/* pp_html_derivable_statement
   DCG for printing the derivable statement with the collapsed
   explanation in html
*/
pp_html_derivable_statement(Name,Seq) -->
    ['The statement <br />
<code>'], pp_Seq(html,Seq),["</code><br />
is derivable. <button class=\"button\" onclick=\"myFunction('fct"],format_name(Name),["')\">Why?</button>

    <div class=\"abstract\" id=\"fct"],format_name(Name),["\">"].


/* pp_html_successors_new
   DCG to print a list of explanations in html as unordered list.
*/
pp_html_successors_new(Depth,Suc) -->
    pp_nl_tab(Depth),
    ["<ul>"],
    pp_html_aux_list_new(Depth + 2,Suc),
    pp_nl_tab(Depth),
    ["</ul>"].


/* pp_html_aux_list_new
   DCG auxiliary to pp_html_successors_new
*/
pp_html_aux_list_new(_,[]) --> [].
pp_html_aux_list_new(Depth,[node(_,no_p_conflict(na))|Tail]) -->
    pp_html_aux_list_new(Depth,Tail).
pp_html_aux_list_new(Depth,[Tree|Tail]) -->
    pp_nl_tab(Depth),
    ["<li>"], pp_derivation(html,Depth + 2,Tree),
    pp_nl_tab(Depth),["</li>"],
    pp_html_aux_list_new(Depth,Tail).


/* pp_html_skip_list_new
 * skips all the propositional rule applications and initial sequent
 * in the derivation and continues with the conclusions of the modal
 * rules / facts.
*/
pp_html_skip_list_new(_,List) -->
    {propositional_derivations(List)},
    ['<li>No assumptions</li>'].
pp_html_skip_list_new(Depth,[Node|Tail]) -->
    pp_html_skip_list_aux_new(Depth,[Node|Tail]).


/* pp_html_skip_list_aux_new
 * auxiliary predicate for pp_html_skip_list_new.
*/
pp_html_skip_list_aux_new(_,[]) --> [].
/*
pp_html_skip_list_aux_new(Depth,[node(_,l,fact,PF,_,_)|Tail])
-->
    pp_nl_tab(Depth),
    ["<li>The assumed fact<br />"],pp_nl_tab(Depth+2),["<code>"],
    pp_Seq_DCG(html,PF),["</code>"],
    pp_nl_tab(Depth),
    ["</li>"],
    pp_html_skip_list_aux_new(Depth,Tail).
*/
/*
pp_html_skip_list_aux_new(Depth,[node(_,l,_,_,_,_)|Tail]) -->
    pp_html_skip_list_aux_new(Depth,Tail).
*/
/*
pp_html_skip_list_aux_new(Depth,[node(Name, p, Modal, PF, Seq, Suc)|Tail]) -->
    {rule_type(Modal,modal)},
    pp_nl_tab(Depth),
    ["<li>"],
    pp_derivation(html,Depth + 2,node(Name, p, Modal, PF, Seq, Suc)),
    pp_nl_tab(Depth),
    ["</li>"],
    pp_html_skip_list_aux_new(Depth,Tail).
pp_html_skip_list_aux_new(Depth,[node(_,p,_,_,_,Suc)|Tail]) -->
    pp_html_skip_list_aux_new(Depth,Suc),
    pp_html_skip_list_aux_new(Depth,Tail).
*/
pp_html_skip_list_aux_new(Depth,[node(_,Rule,_,_,_)|Tail]) -->
    {member(Rule,[init,botL,topR])},
    pp_html_skip_list_aux_new(Depth,Tail).
pp_html_skip_list_aux_new(Depth,[node(_,fact,PF,_,_)|Tail]) -->
    pp_nl_tab(Depth),
    ["<li>The assumed fact<br />"],pp_nl_tab(Depth+2),["<code>"],
    pp_Seq(html,PF),["</code>"],
    pp_nl_tab(Depth),
    ["</li>"],
    pp_html_skip_list_aux_new(Depth,Tail).
pp_html_skip_list_aux_new(Depth,[node(Name,Rule,PF,Seq,Suc)|Tail]) -->
    {rule_type(Rule,modal)},
    pp_nl_tab(Depth),
    ["<li>"],
    pp_derivation(html,Depth + 2,node(Name,Rule,PF,Seq,Suc)),
    pp_nl_tab(Depth),
    ["</li>"],
    pp_html_skip_list_aux_new(Depth,Tail).
pp_html_skip_list_aux_new(Depth,[node(_,Rule,_,_,Suc)|Tail]) -->
    {rule_type(Rule,propositional)},
    pp_html_skip_list_aux_new(Depth,Suc),
    pp_html_skip_list_aux_new(Depth,Tail).
pp_html_skip_list_aux_new(Depth,[node(_,no_p_conflict(na))|Tail])
-->
    pp_html_skip_list_aux_new(Depth,Tail).
pp_html_skip_list_aux_new(Depth,[node(Name,Rule)|Tail]) -->
    pp_nl_tab(Depth),
    ["<li>"],
    pp_derivation(html,Depth + 2,node(Name,Rule)),
    pp_nl_tab(Depth),
    ["</li>"],
    pp_html_skip_list_aux_new(Depth,Tail).
pp_html_skip_list_aux_new(Depth,[node(Name,Rule,Suc)|Tail]) -->
    pp_nl_tab(Depth),
    ["<li>"],
    pp_derivation(html,Depth + 2,node(Name,Rule,Suc)),
    pp_nl_tab(Depth),
    ["</li>"],
    pp_html_skip_list_aux_new(Depth,Tail).


/* pp_html_truncated_new
 * prints a derivation truncated at the modal rules for explanation in
 * html
*/
pp_html_truncated_new(Depth,node(Name,Rule,PF,Seq,Suc)) -->
    {member(Rule,[init,botL,topR,fact])},
    pp_derivation(html,Depth,node([t|Name],Rule,PF,Seq,Suc)).
pp_html_truncated_new(_, node(Name, Rule, _, Seq, _)) -->
    {rule_type(Rule, modal)},
    pp_html_truncated_new_modal([t|Name], Seq).
pp_html_truncated_new(Depth,node(_,disjR,_,_,[Suc])) -->
    pp_html_truncated_new(Depth,Suc).
pp_html_truncated_new(Depth,node(_,conjL,_,_,[Suc])) -->
    pp_html_truncated_new(Depth,Suc).
% NOTE: change this to include the explanations of the propositional rules.
pp_html_truncated_new(Depth,node(Name,Rule,_,Seq,Suc)) -->
    {rule_type(Rule, propositional)},
    pp_html_derivable_statement([t|Name],Seq),
    pp_nl_tab(Depth),
    ["Because it follows from the following using propositional logic:"],
    pp_html_truncated_successors_new(Depth + 2,Suc),
    pp_nl_tab(Depth),
    ["</div>"].


/* pp_html_truncated_new_modal
*/
pp_html_truncated_new_modal(Name,Seq) -->
    pp_html_derivable_statement(Name,Seq),
      ["See above.
      "],
    ["</div>"].


/* pp_html_truncated_successors
 * prints a list of successors in a truncated derivation in html code
 * as an unordered list.
*/
pp_html_truncated_successors_new(Depth,Suc) -->
    pp_nl_tab(Depth),
    ["<ul>"],
    pp_html_truncated_list_new(Depth + 2,Suc),
    pp_nl_tab(Depth),
    ["</ul>"].


/* pp_html_truncated_list_new
 * auxiliary predicate for pp_html_truncated_successors
*/
pp_html_truncated_list_new(_,[]) --> [].
pp_html_truncated_list_new(Depth,[Tree|Tail]) -->
    pp_nl_tab(Depth),
    ["<li>"], pp_html_truncated_new(Depth + 2,Tree),
    pp_nl_tab(Depth),
    ["</li>"],
    pp_html_truncated_list_new(Depth,Tail).



% node(Rule, PF, Seq, Suc)

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
rule_type(pRule(_),modal).


/* pp_tab//1
 * DCG for indenting using N spaces
*/
pp_tab(0) --> [].
pp_tab(N) --> {N =\=0, M is N-1}, [' '], pp_tab(M).


/* pp_nl_tab//1
   DCG for newline followed by N spaces
*/
pp_nl_tab(N) --> ['
'],pp_tab(N).


/* replace_underscores /2
   For replacing prefixing underscores with the escape character.
*/
/*
replace_underscores([]) --> [].
replace_underscores([\u0x5f|List]) --> ['\\_'], replace_underscores(List).
replace_underscores(['_'|List]) --> ['\\_'], replace_underscores(List).
replace_underscores([X|List]) --> [X],replace_underscores(List).
*/
replace_underscores(Atom_in,Atom_out) :-
%    term_to_atom(Term_in,Atom_in),
    name(Atom_in,List_in),
    replace_underscores_aux(List_in,List_out),
    name(Atom_out,List_out).
%    term_string(Term_out,Atom_out).
/*
replace_underscores(Atom_in,Atom_out) :-
    atom(Atom_in),
    name(Atom_in,List_in),
    replace_underscores_aux(List_in,List_out),
    name(Atom_out,List_out).
replace_underscores(Complex_in,Complex_out) :-
    Complex_in =.. [Op|Args],
    maplist(replace_underscores,Args,Args_replaced),
    Complex_aux =.. [Op|Args_replaced],
    term_string(Complex_aux,Complex_out).
*/

replace_underscores_aux([],[]).
replace_underscores_aux([95|Tail_in],[92,95|Tail_out]) :-
    replace_underscores_aux(Tail_in,Tail_out).
replace_underscores_aux([X|Tail_in],[X|Tail_out]) :-
    \+ X == 95,
    replace_underscores_aux(Tail_in,Tail_out).


/* tree_vs_named_tree
 * relates a derivation tree with its named version
 * NOTE: for the new format of derivations
*/
tree_vs_named_tree(nonderivable,nonderivable).
tree_vs_named_tree(Tree,Tree_named) :-
    tree_vs_named_tree_aux([],0,Tree,Tree_named).

/* tree_vs_named_tree_aux
 * takes prefix and current number and relates a derivation tree with
 * its named version.
*/

tree_vs_named_tree_aux(Prefix,N,node(Rule, PF, Seq, Suc),
			   node([M|Prefix], Rule, PF, Seq,
				Suc_Named)) :-
    M is N+1,
    treelist_vs_named_treelist([M|Prefix],0,Suc,Suc_Named).
tree_vs_named_tree_aux(Prefix, N, ndlist(List),
		       node([M|Prefix], ndlist, List_Named)) :-
    M is N+1,
    treelist_vs_named_treelist([M|Prefix],0,List,List_Named).
tree_vs_named_tree_aux(Prefix, N, standard_block(List),
		       node([M|Prefix], standard_block, List_Named)) :-
    M is N+1,
    treelist_vs_named_treelist([M|Prefix],0,List,List_Named).
tree_vs_named_tree_aux(Prefix, N, not_excepted_block(List),
		       node([M|Prefix], not_excepted_block, List_Named)) :-
    M is N+1,
    treelist_vs_named_treelist([M|Prefix],0,List,List_Named).
tree_vs_named_tree_aux(Prefix, N, no_active_conflict_block(List),
		       node([M|Prefix], no_active_conflict_block, List_Named)) :-
    M is N+1,
    treelist_vs_named_treelist([M|Prefix],0,List,List_Named).
tree_vs_named_tree_aux(Prefix,N,node(no_p_conflict(na)),
		       node([M|Prefix],no_p_conflict(na))) :-
    M is N+1.
tree_vs_named_tree_aux(Prefix,N,node(no_p_conflict(Op,Seq)),
		       node([M|Prefix],no_p_conflict(Op,Seq))) :-
    M is N+1.
tree_vs_named_tree_aux(Prefix,N,node(notapplicable(Fml,Seq)),
		       node([M|Prefix],notapplicable(Fml,Seq))) :-
    M is N+1.
tree_vs_named_tree_aux(Prefix,N,node(no_conflict(Fml,Seq)),
		       node([M|Prefix],no_conflict(Fml,Seq))) :-
    M is N+1.
tree_vs_named_tree_aux(Prefix,N,node(noconflict(Fml,Seq)),
		       node([M|Prefix],noconflict(Fml,Seq))) :-
    M is N+1.
tree_vs_named_tree_aux(Prefix,N,node(not_implied(Fml,Seq)),
		       node([M|Prefix],not_implied(Fml,Seq))) :-
    M is N+1.
tree_vs_named_tree_aux(Prefix,N,node(superior(Norm1,Norm2)),
		       node([M|Prefix],superior(Norm1,Norm2))) :-
    M is N+1.
tree_vs_named_tree_aux(Prefix, N, node(not_overruled(Assumption),List),
		       node([M|Prefix], not_overruled(Assumption), List_Named)) :-
    M is N+1,
    treelist_vs_named_treelist([M|Prefix],0,List,List_Named).
tree_vs_named_tree_aux(Prefix, N, node(notoverruled(Assumption,Seq,Suc)),
		       node([M|Prefix], notoverruled(Assumption,Seq,Suc_Named))) :-
    M is N+1,
    treelist_vs_named_treelist([M|Prefix],0,Suc,Suc_Named).
tree_vs_named_tree_aux(Prefix, N, node(overrides(Fml1,Fml2),List),
		       node([M|Prefix], overrides(Fml1,Fml2), List_Named)) :-
    M is N+1,
    treelist_vs_named_treelist([M|Prefix],0,List,List_Named).

% CONTINUE HERE:
/* TODO: Extend this to the following:
   [X] node(not_overruled(Assumption),Tree_list)
   [X] node(notapplicable(Fml,Seq))
   [X] node(noconflict(Fml,Seq))
   [X] node(notimplied(Fml,Seq))
   [X] node(superior(Norm1,Norm2))
   [X] node(overrides(Fml1,Fml2),Succ)
   [X] node(no_p_conflict(Op,Seq))
   [X] node(no_p_conflict(na))
*/


/* treelist_vs_named_treelist
 * takes prefix and current number and relates list of derivation
 * trees with their named versions.
*/
treelist_vs_named_treelist(_,_,[],[]).
treelist_vs_named_treelist(Name,N,[Tree|Tail],[Tree_Named|Tail_Named]) :-
    tree_vs_named_tree_aux(Name,N,Tree,Tree_Named),
    M is N+1,
    treelist_vs_named_treelist(Name,M,Tail,Tail_Named).


/* propositional_derivations
 * true if a list of named derivations does not contain modal rules or
 * factual assumptions.
 * For skipping the propositional steps in printing the explanation in
 * html in pp_html_skip_list_new
*/
% TODO: Check the format of the node!
propositional_derivations([]).
propositional_derivations([node(_,Rule,_,_,Suc)|Tail]) :-
    (rule_type(Rule, propositional)
    ;
    member(Rule,[init,botL,topR])),
    propositional_derivations(Suc),
    propositional_derivations(Tail).

/*
 [ node( [1, 1],
           negR, 
             seq([], [neg at(a)]), 
                seq([], [at(a), neg at(a)]), 
		   [ node([1, 1, 1], init, seq([at(a)], [at(a)]),
		   seq([at(a)], [inv(at(a)), at(a)]), [])
                   ]
   )
 ]
*/

%%% TODO: pretty printing of bb(bla) and b(blub) etc
%%% perhaps even max_measure(.,.,.)
