/* pd7601.pl
 * file for Plangebiet 7601.
*/

bauland(plangebiet(7601)) --> [bauland(7601:1)].

bauland_facts(bauland(7601:1)) --> [].

grundflaechen(bauland(7601:1))
--> [grundflaeche(7601:1/1), grundflaeche(7601:1/2)].

grundflaechen_facts(bauland(7601:1))
--> [grundflaeche(7602:1/1) -> widmungID(test), grundflaeche(7602:1/2)
     -> widmungID(test)].

grundflaechen_obligations(bauland(7601:1))
--> [obl( test1, grundflaeche(7602:1/1)),
     obl( test2, grundflaeche(7602:1/2))].

fluchtlinien(bauland(7601:1)) --> [baulinie(7601:1/1),
				   baulinie(7601:1/2)].

fluchtlinien_facts(bauland(7601:1)) --> [].

textliche_bestimmungen(plangebiet(7601))
--> [obl( test_text_best, plangebiet(7601))]. 

