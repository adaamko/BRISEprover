/*  Plangebiet 7602
 *  Main object: plangebiet(7602)
*/

/* Facts about the plangebiet
*/
facts(Plangebiet,L) :-
    location_facts(L_location),
    bauland_facts(Plangebiet,L_bauland),
    grundflaechen_facts(bauland(7602_1),L_grund),
    fluchtlinien_facts(bauland(7602_1),L_flucht),
    append(L_grund,L_flucht,L1),
    append(L_bauland,L1,L2),
    append(L_location,L2,L).

obligations_plangebiet(plangebiet(7602),L) :-
    grundflaechen_obligations(bauland(7602_1),L_grund),
    textliche_bestimmungen(plangebiet(7602),L_text),
    append(L_text,L_grund,L).
    

/* Bauland auf dem Plangebiet:
 * Given as a list of terms
*/
bauland(plangebiet(7602), [bauland(7602_1)]).
% bauland(7602.1)  % ganz links unten

/* Facts about Bauland
 * Every bauland is in the plangebiet
*/
bauland_facts(plangebiet(7602),[bauland(7602_1) -> plangebiet(7602)]).

/* Grundflaechen auf dem Bauland
 * von der noerdlichsten im Uhrzeigersinn spiralfoermig nach innen
*/
grundflaechen(bauland(7602_1), [grundflaeche(7602_1_
1), grundflaeche(7602_1_2), grundflaeche(7602_1_3), grundflaeche(7602_
1_4), grundflaeche(7602_1_5)]).

/* Facts about the grundflaechen on the bauland
*/
grundflaechen_facts(bauland(7602_1), [
	grundflaeche(7602_1_1) -> bauland(7602_1),
	grundflaeche(7602_1_1) -> widmung(gb),
	grundflaeche(7602_1_1) -> bauklasse(iv),
	grundflaeche(7602_1_1) -> bauweise(g),
	grundflaeche(7602_1_2) -> bauland(7602_1),
	grundflaeche(7602_1_2) -> widmung(g),
	grundflaeche(7602_1_3) -> bauland(7602_1),
	grundflaeche(7602_1_3) -> widmung(gb),
	grundflaeche(7602_1_3) -> bauklasse(vi),
	grundflaeche(7602_1_3) -> bauweise(g),
	grundflaeche(7602_1_4) -> bauland(7602_1),
	grundflaeche(7602_1_4) -> widmung(gb),
	grundflaeche(7602_1_4) -> bauklasse(v),
	grundflaeche(7602_1_4) -> bauweise(g),
	grundflaeche(7602_1_5) -> bauland(7602_1),
	grundflaeche(7602_1_5) -> widmung(gbgv),
	grundflaeche(7602_1_5) -> bauklasse(i),
	grundflaeche(7602_1_5) -> bauweise(g),
	grundflaeche(7602_1_5) -> bb(7602_2)]).

/* obligations about the grundflaechen
*/
grundflaechen_obligations(bauland(7602_1), [
			      obl( max_measure(gebaeude,hoehe,1200),
				   grundflaeche(7602_1_1) ),
			      obl( min_measure(gebaeude, hoehe, 3200)
				   and max_measure( gebaeude, hoehe,
						    3400),
				   grundflaeche(7602_1_3) ),
			      obl( max_measure(gebaeude,hoehe,1200),
				   grundflaeche(7602_1_4) ),
			      obl( max_measure(gebaeude,hoehe,450), grundflaeche(7602_1_5) )]). 


/* Fluchtlinien auf dem Bauland
 * vom noerdlichsten Ende im Uhrzeigersinn.
*/	
fluchtlinie(bauland(7602_1),[baulinie(7602_1_1),
			     baulinie(7602_1_2),
			     baulinie(7602_1_3),
			     baulinie(7602_1_4),
			     baufluchtlinie(7602_1_1),
			     baufluchtlinie(7602_1_2),
			     baufluchtlinie(7602_1_3),
			     baufluchtlinie(7602_1_4),
			     baufluchtlinie(7602_1_5),
			     baufluchtlinie(7602_1_6),
			     grenzlinie(7602_1_1),
			     grenzlinie(7602_1_2),
			     grenzlinie(7602_1_3)
			    ]).

/* Facts about Fluchtlinien
*/
fluchtlinien_facts(bauland(7602_1),[baulinie(7602_1_2) -> bb(7602_4),
		       baulinie(7602_1_3) -> bb(7602_4),
	               baulinie(7602_1_4) -> bb(7602_4)
		   ]).


/* Textliche Bestimmungen (Auswahl):
*/
%% NOTE: "Baulinien begrenzen das Bauland gegenueber oeffentlichen Verkehrsflaechen"
%% also: an_oeffentlicher_Verkehrsflaeche <-> an_baulinie?
textliche_bestimmungen(plangebiet(7602), 
		       [b7602_3_4:for( staffelgeschoss,
				       plangebiet(7602) and
				       an_oeffentlicher_verkehrsflaeche),
			b7602_3_5: for( erker or balkon or loggia,
				       plangebiet(7602) and
				       an_baulinie and
				       max_measure(anliegende_verkehrsflaeche,
						   baulinienabstand,
						   1600)
				     ), 
			%% NOTE: this is going to be rubbish...
			b7602_3_5: per( vorstehende_bauelemente and
				       max_measure(vorstehende_bauelemente,
						   ausladung, 60),
				       plangebiet(7602)
				       and an_baulinie and
				       max_measure(anliegende_verkehrsflaeche,
						   breite, 1600)
				     ), 
			b7602_3_5: per( vorstehende_bauelemente and
				       max_measure(vorstehende_bauelemente,
						   ausladung, 80),
				       plangebiet(7602) and
				       an_baulinie and
				       min_measure(anliegende_verkehrsflaeche,
						   breite, 1600) ), 
			b7602_3_6:for( buero or geschaeft, widmung(w)
				     ), 
			b7602_3_7:
			for( min_measure( dach, hoehe_ueber_gebaeude,
					  450), plangebiet(7602)), 
			bb(7602_1): for( gebauede, bb(7602_1) ),
			bb(7602_2): obl(begruentes_flachdach,
					bb(7602_2) ), 
			bb(7602_3): per( staffelgeschoss,
					 bb(7602_3) and
					 an_strassenfront ), 
			bb(7602_4):
			for(fenster_von_wohnung_zu_verkehrsflaeche,
			    bb(7604) ), 
			bb(7602_5): per(gebaeude and
					max_measure(gebaeude, hoehe,
						    450) and
					max_measure(flaeche, anteil,
						    10),
					bb(7602_5) and (widmung(g) or
							widmung(esp))
				       ),
			bb(7602_7):
			obl( oedf and min_measure(oedf, hoehe, 300),
			     bb(7602_7) ),
			bb(7602_8): obl( max_measure(flaeche, anteil,
						     60), bb(7602_8) )
		       ]).

%% bb(7602_6): skipped this for the time being... 


/* Predicate for automatically adding facts about inclusions based on
 * the name
*/
on_location(Term1,Term2) :-
    objects(L),
    member(Term1,L),
    member(Term2,L),
    Term1 =.. [_,Name1|_],
    Term2 =.. [_,Name2|_],
    \+ Name1 = Name2,
    sub_atom(Name1,0,_,_,Name2).

/* objects
*/
objects([plangebiet(7602),bauland(7602_1)|L]) :- 
    grundflaechen(bauland(7602_1),L1),
    fluchtlinie(bauland(7602_1),L2),
    append(L1,L2,L).
    
location_facts(L) :-
    findall( Term1 -> Term2, on_location(Term1,Term2), L).


% for testing: pretty print a list of terms
pp([]).
pp([Term|List]) :-
    write(Term), nl, pp(List).
