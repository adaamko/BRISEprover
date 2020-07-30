/* Plangebiet X - template
 * Main object: plangebiet(X)
*/

/* bauland//1
 * Bauland auf dem Plangebiet:
 * Given as a list of terms
*/
bauland(plangebiet(X)) --> [bauland(X_1),...].


/* bauland_facts//1
 * Facts about Bauland
 * Every bauland is in the plangebiet NOTE: should be covered by location_facts?
*/
bauland_facts(plangebiet(X)) --> [bauland(X_1) -> plangebiet(X)].
bauland_facts(bauland(X_1)) --> [...].


/* grundflaechen//1
 * Grundflaechen auf dem Bauland
 * von der noerdlichsten im Uhrzeigersinn spiralfoermig nach innen
*/
grundflaechen(bauland(X_1))
--> [grundflaeche(X_1_1), ...].


/* grundflaechen_facts//1
 * Facts about the grundflaechen on the bauland
 * e.g.: grundflaeche(X_1_1) -> widmung(gb);
 * grundflaeche(X_1_1) -> bauklasse(iv) 
 * grundflaeche(X_1_1) -> bauweise(g)
*/
grundflaechen_facts(bauland(X_1)) --> 
    [grundflaeche(X_1_1) -> bauland(X_1), ...].


/* grundflaechen_obligations//1
 * obligations about the grundflaechen on the bauland
 * e.g.:
 * obl( max_measure(gebaeude,hoehe,1200), grundflaeche(X_1_1) )
*/
grundflaechen_obligations(bauland(X_1)) --> [...].


/* fluchtlinien//1
 * Fluchtlinien auf dem Bauland
 * vom noerdlichsten Ende im Uhrzeigersinn.
*/	
fluchtlinien(bauland(X_1)) --> [baulinie(X_1_1),...,
			     baufluchtlinie(X_1_1),...,
			     grenzlinie(X_1_1),...].


/* fluchtlinien_facts//1
 * Facts about Fluchtlinien
 * e.g. baulinie(X_1_2) -> bb(X_4)
*/
fluchtlinien_facts(bauland(X_1)) --> [...].


/* textliche_bestimmungen//1
 * Textliche Bestimmungen.
 * E.g.: b7602_3_4:for(staffelgeschoss,plangebiet(7602) and
 * an_oeffentlicher_verkehrsflaeche), 
 * b7602_3_6:for( buero or geschaeft, widmung(w) ),
 * bb(7602_2): obl(begruentes_flachdach, bb(7602_2) ), 
 * bb(7602_3): per( staffelgeschoss, bb(7602_3) and an_strassenfront ), 
*/
textliche_bestimmungen(plangebiet(X)) --> 
		       [...].




