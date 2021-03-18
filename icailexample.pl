/* Plangebiet icail
 * Example for the ICAIL 2021 article
 * Main object: plangebiet(icail)
*/

/* bauland//1
 * Bauland auf dem Plangebiet:
 * Given as a list of terms
*/
bauland(plangebiet(icail)) --> [].


/* bauland_facts//1
 * Facts about Bauland
 * Every bauland is in the plangebiet NOTE: should be covered by location_facts?
*/
bauland_facts(plangebiet(icail)) --> [bauland(icail_1) -> plangebiet(icail)].
bauland_facts(bauland(icail_1)) --> [].


/* grundflaechen//1
 * Grundflaechen auf dem Bauland
 * von der noerdlichsten im Uhrzeigersinn spiralfoermig nach innen
*/
grundflaechen(bauland(icail_1))
--> [grundflaeche(icail_1_1)].


/* grundflaechen_facts//1
 * Facts about the grundflaechen on the bauland
 * e.g.: grundflaeche(icail_1_1) -> widmung(gb);
 * grundflaeche(icail_1_1) -> bauklasse(iv) 
 * grundflaeche(icail_1_1) -> bauweise(g)
*/
grundflaechen_facts(bauland(icail_1)) --> [].
%    [grundflaeche(icail_1_1) -> bauland(icail_1), ...].


/* grundflaechen_obligations//1
 * obligations about the grundflaechen on the bauland
 * e.g.:
 * obl( max_measure(gebaeude,hoehe,1200), grundflaeche(icail_1_1) )
*/
grundflaechen_obligations(bauland(icail_1)) --> [].


/* fluchtlinien//1
 * Fluchtlinien auf dem Bauland
 * vom noerdlichsten Ende im Uhrzeigersinn.
*/	
fluchtlinien(bauland(icail_1)) --> [].
/*[baulinie(icail_1_1),...,
			     baufluchtlinie(icail_1_1),...,
			     grenzlinie(icail_1_1),...].
*/

/* fluchtlinien_facts//1
 * Facts about Fluchtlinien
 * e.g. baulinie(icail_1_2) -> bb(icail_4)
*/
fluchtlinien_facts(bauland(icail_1)) --> [].


/* textliche_bestimmungen//1
 * Textliche Bestimmungen.
*/
textliche_bestimmungen(plangebiet(icail)) -->
    [b(icail:7181/3/0): obl( gehsteigbreiteMin(200),
			     strassenbreiteMin(1000) and anFluchtlinie
			   ),
     b(icail:7181/6/0): obl( begruenungDach, dachart(flachdach) and
					    dachneigungMax(5) ),
     b(icail:7272/10/9): obl( begruenungDach and dachart(flachdach),
			      planzeichenBBID(bb2) ),
     b(icail:7408/10/1): obl( begruenungDach and dachart(flachdach),
			     gebaeudeBautyp(nebengebaeude) and
			     dachflaecheMin(5) ),
     b(icail:7408/10/1/1):
     per( neg( begruenungDach and dachart(flachdach)),
	  gebaeudeBautyp(nebengebaeude) and dachflaecheMin(5) and
	  dachart(glasdach) ),
     b(icail:7774/18/0): obl( gebaeudeHoeheMax(1100),
			      planzeichenBBID(bb10) and widmungID(esp)
			    ),
     b(icail:7774/18/1): obl( abschlussDachMax(300),
			      gebaeudeHoeheArt(tatsaechlichErrichtet)
			    ),
     b(icail:8159/21/0): obl( gebaeudeHoeheMax(800),
			      planzeichenBBID(bb4) )
    ].

