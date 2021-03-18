/*  Plangebiet 7602
 *  Main object: plangebiet(7602)
 *  New version (with the proper attribute names)
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
	grundflaeche(7602:1/1) -> widmungID(gb),
	grundflaeche(7602:1/1) -> bauklasseID(iv),
	grundflaeche(7602:1/1) -> bauweiseID(g),
	grundflaeche(7602:1/2) -> bauland(7602:1),
	grundflaeche(7602:1/2) -> widmungID(g),
	grundflaeche(7602:1/3) -> bauland(7602:1),
	grundflaeche(7602:1/3) -> widmungID(gb),
	grundflaeche(7602:1/3) -> bauklasseID(vi),
	grundflaeche(7602:1/3) -> bauweiseID(g),
	grundflaeche(7602:1/4) -> bauland(7602:1),
	grundflaeche(7602:1/4) -> widmungID(gb),
	grundflaeche(7602:1/4) -> bauklasseID(v),
	grundflaeche(7602:1/4) -> bauweiseID(g),
	grundflaeche(7602:1/5) -> bauland(7602:1),
	grundflaeche(7602:1/5) -> widmungID(gbgv),
	grundflaeche(7602:1/5) -> bauklasseID(i),
	grundflaeche(7602:1/5) -> bauweiseID(g),
				  grundflaeche(7602:1/5) -> bb(7602:2)].

/* grundflaechen_obligations//1
 * obligations about the grundflaechen on the bauland
*/
grundflaechen_obligations(bauland(7602:1)) --> 
[
			      obl( gebaeudeHoeheMax(1200),
				   grundflaeche(7602:1/1) ),
			      obl( gebaeudeHoeheMin(3200)
				   and gebaeudeHoeheMax(3400),
				   grundflaeche(7602:1/3) ),
			      obl( gebaeudeHoeheMax(1200),
				   grundflaeche(7602:1/4) ),
			      obl( gebaeudeHoeheMax(450), grundflaeche(7602:1/5) )]. 


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
/* NOTE: The new version is slightly problematic, in that we have
 * attributes like "verbotStaffelung", which already incorporate the
 * modal operator into the variable!
 * We'll keep the original "staffelgeschoss" here for the time being
 * and concentrate on the less problematic aspects!
 * Also applies to:
 *   vorbautenVerbot(TEXT) vs for(erker or balkon or loggia,...)
*/
textliche_bestimmungen(plangebiet(7602)) --> 
		       [
			   % NOTE: problematic because of
			   % staffelgeschoss (s.a.):
			   b(7602:3/4):for( staffelgeschoss,
				       plangebiet(7602) and
				       anOeffentlichenVerkehrsflaechen
					  ),
			   % NOTE: problematic because of
			   % vorbautenVerbot (s.a.) and because of
			   % max_measure(...,baulinienabstand,...):
			   b(7602:3/5): for( erker or balkon or loggia,
				       plangebiet(7602) and
				       anBaulinie and
				       max_measure(anliegende_verkehrsflaeche,
						   baulinienabstand,
						   1600)
				     ), 
			b(7602:3/5): per( vorstehende_bauelemente and
					vorstehendeBauelementeAusladungMax(60),
				       plangebiet(7602)
				       and anBaulinie and
				       strassenbreiteMax(1600)
				     ), 
			b(7602:3/5): per( vorstehende_bauelemente and
					vorstehendeBauelementeAusladungMax(60),
				       plangebiet(7602) and
				       anBaulinie and
				       strassenbreiteMin(1600)
					), 
			% NOTE: problematic because of buero or
			% geschaeft vs. unzulaessigBueroGeschaeftsgebaeude:
			b(7602:3/6):for( buero or geschaeft, plangebiet(7602) and widmungID(w)
				     ), 
			b(7602:3/7):obl( abschlussDachMax(450),
					 plangebiet(7602)),
			bb(7602:1): obl( vonBebauungFreizuhalten, plangebiet(7602) and bb(7602:1) ),
			bb(7602:2): obl( begruenungDach and dachart(flachdach),
					plangebiet(7602) and bb(7602:2) ), 
			bb(7602:3): per( staffelgeschoss,
					 plangebiet(7602) and bb(7602:3) and
					 anOeffentlichenVerkehrsflaechen ), 
			bb(7602:4):
			% NOTE: replaced
			% unzulaessigFensterZuOeffentlichenVerkehrsflaechen
			% with for(fensterZuOeffentlichenVerkehrsflaechen,...)
			for(fensterZuOeffentlichenVerkehrsflaechen,
			    plangebiet(7602) and bb(7602:4) ), 
			% NOTE: replaced the "gebaeude" with "bebauung"
			bb(7602:5): per(bebauung and
				       gebaeudeHoeheMax(450) and
				       bbAusnuetzbarkeitFlaecheGrundflaechenbezugRelativ(10),
					plangebiet(7602) and bb(7602:5) and (widmungID(g) or
							widmungID(esp))
				       ),
			bb(7602:7):
			obl( durchfahrtHoehe(300), plangebiet(7602) and bb(7602:7) ),
			bb(7602:8): obl( bbBebaubareFlaecheAbgegrenzt(60), plangebiet(7602) and bb(7602:8) ),
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


