/* Bauordnung
 * Some clauses from the Wiener Bauordnung.
 * as DCG
*/

bauordnung(b) --> bauordnung(75).

/* paragraph 75: Bauklasseneinteilung
*/
/* NOTE: Par.75(4)-(9) are still missing
*/
bauordnung(75) --> [obl( min_measure(building,height,250), bauklasse(i) ),
		    obl( max_measure(building,height,900), bauklasse(i) ),
		    obl( min_measure(building,height,250), bauklasse(ii) ),
		    obl( max_measure(building,height,1200), bauklasse(ii) ),
		    obl( min_measure(building,height,900), bauklasse(iii) ),
		    obl( max_measure(building,height,1600), bauklasse(iii) ),
		    obl( min_measure(building,height,1200), bauklasse(iv) ),
		    obl( max_measure(building,height,2100), bauklasse(iv) ),
		    obl( min_measure(building,height,1600), bauklasse(v) ),
		    obl( max_measure(building,height,2600), bauklasse(v) ),
		    obl( min_measure(building,height,2100), bauklasse(vi) )].
