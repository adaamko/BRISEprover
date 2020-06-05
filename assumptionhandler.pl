/* assumptionhandler.pl
 * Predicates / DCG for constructing and manipulating the factual and
 * deontic assumptions from the Bebauungsplan.
*/

/* facts_plangebiet//1
 * generates all the factual assumptions about a Plangebiet, including
 * the location-based ones, and the ones about the Bauland,
 * Grundflaechen and Fluchtlinien.
 * bauland(.) specifies the Bauland on the Plangebiet and is given in the
 * file for the specific Plangebiet.
*/
facts_plangebiet(Plangebiet) -->
    location_facts([Plangebiet]),
    {phrase(bauland(Plangebiet),Bs)},
    lift_DCG(combined_facts,Bs).

/* combined_facts//1
 * combines facts about the Bauland, the Grundflaechen and the
 * Fluchtlinien on that Bauland.
 * bauland_facts, grundflaechen_facts and fluchtlinien_facts are given
 * in the file for the specific Plangebiet.
*/
combined_facts(Bauland)
--> bauland_facts(Bauland), 
    grundflaechen_facts(Bauland),
    fluchtlinien_facts(Bauland).

/* obligations_plangebiet//1
 * generates the list of obligations for all Grunddflaechen on the
 * Bauland on the Plangebiet together with the textliche Bestimmungen.
 * bauland(.), grundflaechen_obligations(.) and
 * textliche_bestimmungen(.) are given in the file for the specific
 * Plangebiet.
*/
obligations_plangebiet(Plangebiet)
--> {phrase(bauland(Plangebiet),Bs)},
    lift_DCG(grundflaechen_obligations,Bs),
    textliche_bestimmungen(Plangebiet).
    
/* on_location /3
 * true if Term1 and Term2 are objects in a location in List and Term1
 * is located on Term2
 * NOTE: this uses the naming convention that objects are named by
 * following the schem Plangebiet_Bauland_Object, hence "being located
 * on" is checked using the syntactic prefix relation on the names of
 * objects.
*/
on_location(List,Term1,Term2) :-
    phrase(objects(List),Objects),
    member(Term1,Objects),
    member(Term2,Objects),
    Term1 =.. [_,Name1|_],
    Term2 =.. [_,Name2|_],
    \+ Name1 = Name2,
    sub_atom(Name1,0,_,_,Name2).

/* objects//1
 * Given a list of Plangebiete as parameter generates the list of
 * objects on those Plangebiete, i.e., the Plangebiete, the Baulande,
 * the Grundflaechen, the Fluchtlinien.
*/
objects([]) --> [].
objects([Plangebiet|Tail]) --> [Plangebiet],
			       {phrase(bauland(Plangebiet),Bauland,[])
			       },
			       Bauland,
			       lift_DCG(grundflaechen,Bauland),
			       lift_DCG(fluchtlinien,Bauland),
			       objects(Tail).


/* location_facts//1
 * DCG for generating all the facts about objects on a Plangebiet.
*/
location_facts(Plangebiet_list) -->
    {findall( Term1 -> Term2, on_location(Plangebiet_list,Term1,Term2),
		       Facts)}, Facts.


% for testing: pretty print a list of terms
pp_tester([]).
pp_tester([Term|List]) :-
    write(Term), nl, pp_tester(List).
