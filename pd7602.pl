/*  Plangebiet 7602
 *  Main object: plangebiet(7602)
*/


/* bauland//1
 * DCG for Bauland auf dem Plangebiet:
 * Given as a list of terms
*/
bauland(plangebiet(7602)) --> [bauland(7602_1)].
% bauland(7602.1)  % ganz links unten
% For testing purposes:
bauland(test(1)) --> [p(test_1)].


/* bauland_facts//1
 * Facts about Bauland
 * Every bauland is in the plangebiet NOTE: should be covered by location_facts?
*/
bauland_facts(plangebiet(7602)) --> [bauland(7602_1) -> plangebiet(7602)].
bauland_facts(bauland(7602_1)) --> [].


/* grundflaechen//1
 * Grundflaechen auf dem Bauland
 * von der noerdlichsten im Uhrzeigersinn spiralfoermig nach innen
*/
grundflaechen(bauland(7602_1))
--> [grundflaeche(7602_1_1), grundflaeche(7602_1_2),
     grundflaeche(7602_1_3), grundflaeche(7602_1_4),
     grundflaeche(7602_1_5)].


/* grundflaechen_facts//1
 * Facts about the grundflaechen on the bauland
*/
grundflaechen_facts(bauland(7602_1)) --> 
    [grundflaeche(7602_1_1) -> bauland(7602_1),
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
				  grundflaeche(7602_1_5) -> bb(7602_2)].

/* grundflaechen_obligations//1
 * obligations about the grundflaechen on the bauland
*/
grundflaechen_obligations(bauland(7602_1)) --> %[].
[
			      obl( max_measure(gebaeude,hoehe,1200),
				   grundflaeche(7602_1_1) ),
			      obl( min_measure(gebaeude, hoehe, 3200)
				   and max_measure( gebaeude, hoehe,
						    3400),
				   grundflaeche(7602_1_3) ),
			      obl( max_measure(gebaeude,hoehe,1200),
				   grundflaeche(7602_1_4) ),
			      obl( max_measure(gebaeude,hoehe,450), grundflaeche(7602_1_5) )]. 


/* fluchtlinien//1
 * Fluchtlinien auf dem Bauland
 * vom noerdlichsten Ende im Uhrzeigersinn.
*/	
fluchtlinien(bauland(7602_1)) --> [baulinie(7602_1_1),
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
			    ].


/* fluchtlinien_facts//1
 * Facts about Fluchtlinien
*/
fluchtlinien_facts(bauland(7602_1)) --> [baulinie(7602_1_2) -> bb(7602_4),
		       baulinie(7602_1_3) -> bb(7602_4),
	               baulinie(7602_1_4) -> bb(7602_4)
		   ].


/* Textliche Bestimmungen (Auswahl):
*/
%% NOTE: "Baulinien begrenzen das Bauland gegenueber oeffentlichen Verkehrsflaechen"
%% also: an_oeffentlicher_Verkehrsflaeche <-> an_baulinie?
textliche_bestimmungen(plangebiet(7602)) --> 
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
			    bb(7602_4) ), 
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
						     60), bb(7602_8) ),
			bb(test): for(a, b),
			bb(test): per(a, d)
		       ].

%% bb(7602_6): skipped this for the time being... 



/* facts_plangebiet_list//1
 * DCG
 * Generates a list with all factual assumptions about objects on the
 * Plangebiete of the input list
*/
facts_plangebiet_list([]) --> [].
facts_plangebiet_list([Plangebiet|Tail])
--> facts_plangebiet(Plangebiet), obligations_plangebiet_list(Tail).

/* obligations_plangebiet_list//1
 * DCG
 * Generates a list with all deontic assumptions about objects of the
 * Plangebiete of the input list.
*/
obligations_plangebiet_list([]) --> [].
obligations_plangebiet_list([Plangebiet|Tail])
--> obligations_plangebiet(Plangebiet), obligations_plangebiet_list(Tail).


% for testing: pretty print a list of terms
/*
pp([]).
pp([Term|List]) :-
    write(Term), nl, pp(List).
*/


/* For testing: imaginary plangebiet(7601)
*/

/*
bauland(plangebiet(7601)) --> [bauland(7601_1)].

%bauland_facts(plangebiet(7601)) --> [].
bauland_facts(bauland(7601_1)) --> [].

grundflaechen(bauland(7601_1))
--> [grundflaeche(7601_1_1), grundflaeche(7601_1_2)].

grundflaechen_facts(bauland(7601_1))
--> [grundflaeche(7602_1_1) -> widmung(test), grundflaeche(7602_1_2)
     -> widmung(test)].

grundflaechen_obligations(bauland(7601_1))
--> [obl( test1, grundflaeche(7602_1_1)),
     obl( test2, grundflaeche(7602_1_2))].

fluchtlinien(bauland(7601_1)) --> [baulinie(7601_1_1),
				   baulinie(7601_1_2)].

fluchtlinien_facts(bauland(7601_1)) --> [].

textliche_bestimmungen(plangebiet(7601))
--> [obl( test_text_best, plangebiet(7601))]. 



*/
