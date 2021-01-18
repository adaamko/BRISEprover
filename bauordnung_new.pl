/* Bauordnung
 * Some clauses from the Wiener Bauordnung.
 * as DCG
 * New format with the atoms matching the attributes.
*/

bauordnung(b) --> bauordnung(54), bauordnung(75).


/* paragraph 54: Gehsteige
*/
bauordnung(54) --> [bo(54:7): obl( gehsteigbreiteMax( 200), bauklasseID(i) ),
		    bo(54:7): obl( gehsteigbreiteMax( 200), widmungID(gs) ),
		    bo(54:7): obl( gehsteigbreiteMax( 200), widmungID(gsgm) ),
		    bo(54:7): obl( gehsteigbreiteMax( 300), bauklasseID(ii) ),
		    bo(54:7): obl( gehsteigbreiteMax( 300), widmungID(ig) ),
		    bo(54:7): obl( gehsteigbreiteMax( 300), widmungID(igbs) ),
		    bo(54:7): obl( gehsteigbreiteMax( 300), widmungID(igsi) ),
		    bo(54:7): obl( gehsteigbreiteMax( 400), bauklasseID(iii) ),
		    bo(54:7): obl( gehsteigbreiteMax( 500), bauklasseID(iiv) ),
		    bo(54:7): obl( gehsteigbreiteMax( 500), bauklasseID(v) ),
		    bo(54:7): obl( gehsteigbreiteMax( 500), bauklasseID(vi) )].


/* paragraph 75: Bauklasseneinteilung
*/
/* NOTE: Par.75(4)-(9) are still missing
*/
bauordnung(75) --> [bo(75:2): obl( gebaeudeHoeheMin(250), bauklasseID(i) ),
		    bo(75:2): obl( gebaeudeHoeheMax(900), bauklasseID(i) ),
		    bo(75:2): obl( gebaeudeHoeheMin(250), bauklasseID(ii) ),
		    bo(75:2): obl( gebaeudeHoeheMax(1200), bauklasseID(ii) ),
		    bo(75:2): obl( gebaeudeHoeheMin(900), bauklasseID(iii) ),
		    bo(75:2): obl( gebaeudeHoeheMax(1600), bauklasseID(iii) ),
		    bo(75:2): obl( gebaeudeHoeheMin(1200), bauklasseID(iv) ),
		    bo(75:2): obl( gebaeudeHoeheMax(2100), bauklasseID(iv) ),
		    bo(75:2): obl( gebaeudeHoeheMin(1600), bauklasseID(v) ),
		    bo(75:2): obl( gebaeudeHoeheMax(2600), bauklasseID(v) ),
		    bo(75:3): obl( gebaeudeHoeheMin(2100), bauklasseID(vi) ),
		    bo(75:7): obl( gebaeudeHoeheMax(550), widmungID(gs) )
		   ].
