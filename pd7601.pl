/* pd7601.pl
 * file for Plangebiet 7601.
*/

bauland(plangebiet(7601)) --> [bauland(7601_1)].

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

