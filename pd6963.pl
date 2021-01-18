/* Plangebiet 6963
 * Main object: plangebiet(6963)
*/

/* bauland//1
 * Bauland auf dem Plangebiet:
 * Given as a list of terms
*/
bauland(plangebiet(6963)) --> [].


/* bauland_facts//1
 * Facts about Bauland
 * Every bauland is in the plangebiet NOTE: should be covered by location_facts?
*/
bauland_facts(plangebiet(6963)) --> [bauland(6963_1) -> plangebiet(6963)].
bauland_facts(bauland(6963_1)) --> [].


/* grundflaechen//1
 * Grundflaechen auf dem Bauland
 * von der noerdlichsten im Uhrzeigersinn spiralfoermig nach innen
*/
grundflaechen(bauland(6963_1))
--> [grundflaeche(6963_1_1)].


/* grundflaechen_facts//1
 * Facts about the grundflaechen on the bauland
 * e.g.: grundflaeche(6963_1_1) -> widmung(gb);
 * grundflaeche(6963_1_1) -> bauklasse(iv) 
 * grundflaeche(6963_1_1) -> bauweise(g)
*/
grundflaechen_facts(bauland(6963_1)) --> [].
%    [grundflaeche(6963_1_1) -> bauland(6963_1), ...].


/* grundflaechen_obligations//1
 * obligations about the grundflaechen on the bauland
 * e.g.:
 * obl( max_measure(gebaeude,hoehe,1200), grundflaeche(6963_1_1) )
*/
grundflaechen_obligations(bauland(6963_1)) --> [].


/* fluchtlinien//1
 * Fluchtlinien auf dem Bauland
 * vom noerdlichsten Ende im Uhrzeigersinn.
*/	
fluchtlinien(bauland(6963_1)) --> [].
/*[baulinie(6963_1_1),...,
			     baufluchtlinie(6963_1_1),...,
			     grenzlinie(6963_1_1),...].
*/

/* fluchtlinien_facts//1
 * Facts about Fluchtlinien
 * e.g. baulinie(6963_1_2) -> bb(6963_4)
*/
fluchtlinien_facts(bauland(6963_1)) --> [].


/* textliche_bestimmungen//1
 * Textliche Bestimmungen.
 * E.g.: b7602_3_4:for(staffelgeschoss,plangebiet(7602) and
 * an_oeffentlicher_verkehrsflaeche), 
 * b7602_3_6:for( buero or geschaeft, widmung(w) ),
 * bb(7602_2): obl(begruentes_flachdach, bb(7602_2) ), 
 * bb(7602_3): per( staffelgeschoss, bb(7602_3) and an_strassenfront ), 
*/
textliche_bestimmungen(plangebiet(6963)) -->
    [b(6963:6/1):obl( gehsteigbreiteMin(80), strassenbreiteMax(1000)
					     and anFluchtlinie and plangebiet(6963)),
     b(6963:6/2):obl( gehsteigbreiteMin(150),
		      strassenbreiteVonBis(1000,1600) and
		      anFluchtlinie and plangebiet(6963)),
     b(6963:6/3):obl( gehsteigbreiteMin(200), strassenbreiteMin(1600)
					     and anFluchtlinie and plangebiet(6963)),
     b(6963:7/1):
     obl( vorkehrungBepflanzungOeffentlicheVerkehrsflaeche(baumreihe),
	  verkehrsflaecheID(in_der_Wiedener_Hauptstrasse_auf_Seite_der_ungeraden_Ordnungsnummern) and plangebiet(6963)),
     b(6963:7/2):
     obl( vorkehrungBepflanzungOeffentlicheVerkehrsflaeche(baumreihe),
	  verkehrsflaecheID(in_der_Graf_Starhemberg_Gasse_auf_Seite_der_geraden_Ordnungsnummern_zwischen_Waltergasse_und_Rainergasse) and plangebiet(6963)),
     b(6963:7/2):
     obl( vorkehrungBepflanzungOeffentlicheVerkehrsflaeche(baumreihe),
	  verkehrsflaecheID(in_der_Mayerhofgasse_an_beiden_Strassenseiten) and plangebiet(6963)),
     b(6963:10):for( vorbauten, anOeffentlichenVerkehrsflaechen and
				plangebiet(6963)),
     b(6963:11/1): per( vorstehendeBauelementeAusladungMax(60),
			strassenbreiteMax(1600) and plangebiet(6963)),
     b(6963:11/2): per( vorstehendeBauelementeAusladungMax(80),
			strassenbreiteMin(1600) and plangebiet(6963)),
     b(6963:12) : for( staffelung, anOeffentlichenVerkehrsflaechen and plangebiet(6963)),
     b(6963:13) : obl( vorkehrungBepflanzung,
		       anordnungGaertnerischeAusgestaltung and
		       unterirdischeBaulichkeiten and plangebiet(6963)),
     b(6963:14) :
     obl( bbBebaubareFlaechefuerNebengebaeudeJeBauplatzMax(30),
	  gebaeudeBautyp(nebengebaeude)  and plangebiet(6963)),
     b(6963:15/1) : obl( begruenungDach and dachart(flachdach),
		       gebaeudeBautyp(nebengebaeude) and
		       dachflaecheMin(5)  and plangebiet(6963)),
     b(6963:15/2) : per( technischeUndBelichtungsaufbauten, 
		       gebaeudeBautyp(nebengebaeude) and
		       dachflaecheMin(5)  and plangebiet(6963)),
     b(6963:17) : obl( abschlussDachMax(450),
		       gebaeudeHoeheArt(tatsaechlich_errichtet) and plangebiet(6963)),
     bb(6963:19/1) : obl(begruenungDach, planzeichenBBID(bb1) and plangebiet(6963)),
     bb(6963:19/2) : per(technischeUndBelichtungsaufbauten,
			planzeichenBBID(bb1) and plangebiet(6963)),
     bb(6963:21/1) : obl( anordnungGaertnerischeAusgestaltung,
		       planzeichenBBID(bb1)  and plangebiet(6963)),
     bb(6963:21/1) : per( ausnahmeGaertnerischAuszugestaltende(manipulations_und_Rangierflaechen),
			 planzeichenBBID(bb1)  and plangebiet(6963)),
     bb(6963:22) : for( fensterZuOeffentlichenVerkehrsflaechen,
			stockwerk(erdgeschoss) and
			anOeffentlicheVerkehrsflaechen and
			planzeichenBBID(bb2) and anBaulinie and plangebiet(6963)),
     bb(6963:23) : obl( weitereBestimmungPruefungErforderlich,
			planzeichenBBID(bb3) and
			gebaeudeBautyp(hauptgebaeude) and plangebiet(6963)),
     bb(6963:24) : for( errichtungGebaeude, planzeichenBBID(bb3) and
					    gebaeudeBautyp(nebengebaeude) and plangebiet(6963)),
     bb(6963:25) : obl(anordnungGaertnerischeAusgestaltung,
		       planzeichenBBID(bb3) and plangebiet(6963)),
     bb(6963:26) :
     obl( bbAusnuetzbarkeitFlaecheGrundflaechenbezugRelativ(95) and
	  anordnungGaertnerischeAusgestaltung and
	  oberflaecheBestimmungP, planzeichenBBID(bb4) and
				  widmungID(p) and plangebiet(6963)),
     bb(6963:27) : obl( abschlussDachMax(400),
			planzeichenBBID(bb6) and
			gebaeudeHoeheArt(tatsaechlich_ausgefuehrt)  and plangebiet(6963)),
     bb(6963:28) : for( unterirdischeBauwerke,
			planzeichenBBID(bb7) and widmungID(g) and plangebiet(6963)),
     bb(6963:29/1) : obl( weitereBestimmungPruefungErforderlich and
			ausnahmePruefungErforderlich,
			widmungID(wohnzohne) and plangebiet(6963)),
     bb(6963:29/2) : per( neg (weitereBestimmungPruefungErforderlich
			       and ausnahmePruefungErforderlich),
			  verkehrsflaecheID(bauplaetze_an_den_Hauptverkehrsstrassen_gemaess_verordnung_38)
			  and stockwerk(erdgeschoss) and plangebiet(6963))
		      
    ].


/* additional constitutional norms from the textuelle Bestimmungen:
 * NOTE: not yet clear how this is implemented!
*/
constitutional_norms(plangebiet(6963)) -->
    [bb(6963:31/1) : (planzeichenBBID(bb5) and plangebiet(6963)
     -> widmungErsteEbene(oeffentliche_Verkehrsflaeche) and
	widmungErsteEbeneBezugObjekt(bis_zur_Konstruktionsunterkante_seiner_Ueberbauung)),
     bb(6963:31/2) : (planzeichenBBID(bb5) and plangebiet(6963)
		     -> widmungZweiteEbene(wohngebiet) and bauklasseID(iv))
    ].
