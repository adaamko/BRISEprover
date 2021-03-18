/*  Plangebiet 7602
 *  Main object: plangebiet(7602)
*/


/* bauland//1
 * DCG for Bauland auf dem Plangebiet:
 * Given as a list of terms
*/
bauland(plangebiet(7602)) --> [bauland(7602:1)].
% bauland(7602.1)  % ganz links unten
% For testing purposes:
bauland(test(1)) --> [p(test_1)].


/* bauland_facts//1
 * Facts about Bauland
 * Every bauland is in the plangebiet 
*/
bauland_facts(plangebiet(7602)) --> [bauland(7602:1) -> plangebiet(7602)].
bauland_facts(bauland(7602:1)) --> [].


/* grundflaechen//1
 * Grundflaechen auf dem Bauland
 * von der noerdlichsten im Uhrzeigersinn spiralfoermig nach innen
*/
grundflaechen(bauland(7602:1))
--> [grundflaeche(7602:1/1), grundflaeche(7602:1/2),
     grundflaeche(7602:1/3), grundflaeche(7602:1/4),
     grundflaeche(7602:1/5)].


/* grundflaechen_facts//1
 * Facts about the grundflaechen on the bauland
*/
grundflaechen_facts(bauland(7602:1)) --> 
    [grundflaeche(7602:1/1) -> bauland(7602:1),
	grundflaeche(7602:1/1) -> widmung(gb),
	grundflaeche(7602:1/1) -> bauklasse(iv),
	grundflaeche(7602:1/1) -> bauweise(g),
	grundflaeche(7602:1/2) -> bauland(7602:1),
	grundflaeche(7602:1/2) -> widmung(g),
	grundflaeche(7602:1/3) -> bauland(7602:1),
	grundflaeche(7602:1/3) -> widmung(gb),
	grundflaeche(7602:1/3) -> bauklasse(vi),
	grundflaeche(7602:1/3) -> bauweise(g),
	grundflaeche(7602:1/4) -> bauland(7602:1),
	grundflaeche(7602:1/4) -> widmung(gb),
	grundflaeche(7602:1/4) -> bauklasse(v),
	grundflaeche(7602:1/4) -> bauweise(g),
	grundflaeche(7602:1/5) -> bauland(7602:1),
	grundflaeche(7602:1/5) -> widmung(gbgv),
	grundflaeche(7602:1/5) -> bauklasse(i),
	grundflaeche(7602:1/5) -> bauweise(g),
				  grundflaeche(7602:1/5) -> bb(7602:2)].

/* grundflaechen_obligations//1
 * obligations about the grundflaechen on the bauland
*/
grundflaechen_obligations(bauland(7602:1)) --> 
[
			      obl( max_measure(gebaeude,hoehe,1200),
				   grundflaeche(7602:1/1) ),
			      obl( min_measure(gebaeude, hoehe, 3200)
				   and max_measure( gebaeude, hoehe,
						    3400),
				   grundflaeche(7602:1/3) ),
			      obl( max_measure(gebaeude,hoehe,1200),
				   grundflaeche(7602:1/4) ),
			      obl( max_measure(gebaeude,hoehe,450), grundflaeche(7602:1/5) )]. 


/* fluchtlinien//1
 * Fluchtlinien auf dem Bauland
 * vom noerdlichsten Ende im Uhrzeigersinn.
*/	
fluchtlinien(bauland(7602:1)) --> [baulinie(7602:1/1),
			     baulinie(7602:1/2),
			     baulinie(7602:1/3),
			     baulinie(7602:1/4),
			     baufluchtlinie(7602:1/1),
			     baufluchtlinie(7602:1/2),
			     baufluchtlinie(7602:1/3),
			     baufluchtlinie(7602:1/4),
			     baufluchtlinie(7602:1/5),
			     baufluchtlinie(7602:1/6),
			     grenzlinie(7602:1/1),
			     grenzlinie(7602:1/2),
			     grenzlinie(7602:1/3)
			    ].


/* fluchtlinien_facts//1
 * Facts about Fluchtlinien
*/
fluchtlinien_facts(bauland(7602:1)) --> [baulinie(7602:1/2) -> bb(7602:4),
		       baulinie(7602:1/3) -> bb(7602:4),
	               baulinie(7602:1/4) -> bb(7602:4)
		   ].


/* Textliche Bestimmungen (Auswahl):
*/
%% NOTE: "Baulinien begrenzen das Bauland gegenueber oeffentlichen Verkehrsflaechen"
%% so: an_oeffentlicher_Verkehrsflaeche <-> an_baulinie?
textliche_bestimmungen(plangebiet(7602)) --> 
		       [b(7602:3/4):for( staffelgeschoss,
				       plangebiet(7602) and
				       an_oeffentlicher_verkehrsflaeche),
			b(7602:3/5): for( erker or balkon or loggia,
				       plangebiet(7602) and
				       an_baulinie and
				       max_measure(anliegende_verkehrsflaeche,
						   baulinienabstand,
						   1600)
				     ), 
			b(7602:3/5): per( vorstehende_bauelemente and
				       max_measure(vorstehende_bauelemente,
						   ausladung, 60),
				       plangebiet(7602)
				       and an_baulinie and
				       max_measure(anliegende_verkehrsflaeche,
						   breite, 1600)
				     ), 
			b(7602:3/5): per( vorstehende_bauelemente and
				       max_measure(vorstehende_bauelemente,
						   ausladung, 80),
				       plangebiet(7602) and
				       an_baulinie and
				       min_measure(anliegende_verkehrsflaeche,
						   breite, 1600) ), 
			b(7602:3/6):for( buero or geschaeft, widmung(w)
				     ), 
			b(7602:3/7):
			for( min_measure( dach, hoehe_ueber_gebaeude,
					  450), plangebiet(7602)), 
			bb(7602:1): for( gebauede, bb(7602:1) ),
			bb(7602:2): obl(begruentes_flachdach,
					bb(7602:2) ), 
			bb(7602:3): per( staffelgeschoss,
					 bb(7602:3) and
					 an_strassenfront ), 
			bb(7602:4):
			for(fenster_von_wohnung_zu_verkehrsflaeche,
			    bb(7602:4) ), 
			bb(7602:5): per(gebaeude and
					max_measure(gebaeude, hoehe,
						    450) and
					max_measure(flaeche, anteil,
						    10),
					bb(7602:5) and (widmung(g) or
							widmung(esp))
				       ),
			bb(7602:7):
			obl( oedf and min_measure(oedf, hoehe, 300),
			     bb(7602:7) ),
			bb(7602:8): obl( max_measure(flaeche, anteil,
						     60), bb(7602:8) ),
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


 
