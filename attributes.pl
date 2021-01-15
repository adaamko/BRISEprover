/*
Copyright 2021 Bjoern Lellmann

    This file is part of BRISEprover.

    BRISEprover is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    BRISEprover is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BRISEprover.  If not, see <http://www.gnu.org/licenses/>.
*/

/* attributes.pl
   Contains background information about the propositional variables /
   attributes used in the annotation of the textliche Bestimmungen in
   the context of BRISE Vienna.
*/


/* List of additional variables to be used with arguments
*/
variable_with_arguments(Op) :-
    (member(Op,[height, width, max_height, max_width, min_height,
	       min_width, measure, max_measure, min_measure, area,
	       plangebiet, bauland, grundflaeche, widmung, bauklasse,
	       bauweise, bb, b, baulinie, baufluchtlinie, grenzlinie,
	       bauweiseID, gebaeudeBautyp, dachart,
	       vorkehrungBepflanzungOeffentlicheVerkehrsflaeche,
	       stockwerk, bauklasseID, gebaeudeHoeheArt,
	       planzeichenBBID, widmungID, widmungErsteEbene,
	       widmungZweiteEbene, widmungDritteEbene,
	       widmungErsteEbeneBezugHoehe,
	       widmungZweiteEbeneBezugHoehe,
	       widmungDritteEbeneBezugHoehe, strassenbreiteVonBis,
	       verkehrsflaecheID, ausnahmeGaertnerischAuszugestaltende
	       ])
    ;
    measuretriple(Op,_,_)
    ;
    measuretriple(_,Op,_)
    ;
    measuretriple(_,_,Op)).
variable_with_arguments_old(Op) :-
    member(Op,[height, width, max_height, max_width, min_height,
	       min_width, measure, max_measure, min_measure, area,
	       plangebiet, bauland, grundflaeche, widmung, bauklasse,
	       bauweise, bb, b, baulinie, badufluchtlinie, grenzlinie,
	       anteilBaumbepflanzung, technischeAufbautenHoeheMax,
	       technischeAufbautenHoeheMin, technischeAufbautenHoehe,
	       technischeAufbautenZulaessig,
	       bauweiseID,
	       unterbrechungGeschlosseneBauweise,
	       unzulaessigkeitUnterirdischeBauwerke, verbotStaffelung,
	       abschlussDachMax, anteilDachbegruenung,
	       bbDachneigungMax, bbDachneigungMin, dachflaecheMin,
	       dachneigungMax,
	       technischeUndBelichtungsAufbautenZulaessig,
	       einfriedungHoeheGesamt, einfriedungHoeheSockel,
	       einfriedungZulaessig, bauplatzUnterirdischeBebauungMax
	       , bbAusnuetzbarkeitFlaecheBGF,
	       bbAusnuetzbarkeitFlaecheBGFRelativ,
	       bbAusnuetzbarkeitFlaecheGrundflaechenbezug,
	       bbAusnuetzbarkeitFlaecheGrundflaechenbezugRelativ,
	       bbAusnuetzbarkeitFlaecheNutzflaeche,
	       bbAusnuetzbarkeitFlaecheNutzflaecheRelativ,
	       bbAusnuetzbarkeitFlaecheWohnnutzflaeche,
	       bbAusnuetzbarkeitFlaecheWohnnutzflaecheRelativ,
	       bbBebaubareFlaecheAbgegrenzt,
	       bbBebaubareFlaecheGesamterBauplatz,
	       bbBebaubareFlaecheJeBauplatz,
	       bbBebaubareFlaecheJeGebaeude,
	       bbBebaubareFlaechefuerNebengebaeudeJeBauplatzMax,
	       bbBebaubareFlaechefuerNebengebaeudeJeBaulosMax,
	       flaecheBebaubar, flaecheBebaut,
	       maxAnzahlGeschosseOberirdisch,
	       maxAnzahlGeschosseOberirdischDachgeschoss,
	       maxAnzahlGeschosseOberirdischOhneDachgeschoss,
	       stockwerk, unterirdischeBaulichkeiten,
	       zulaessigeGeschossanzahl, einkaufszentrumMaxFlaeche,
	       grossbauvorhabenMaxFlaeche,
	       hochhausUnzulaessigGemaessBB,
	       hochhausZulaessigGemaessBB, anschlussGebaeudeAnGelaende
	       , bauklasseID, bbBauklasseMaximum, bbBauklasseMinimum,
	       fbokMinimumWohnungen, gebaeudeHoeheArt,
	       gebaeudeHoeheBeschraenkung, gebaeudeHoeheMax,
	       maxHoeheWohngebaeude, mindestraumhoeheEG, anBaulinie,
	       anFluchtlinie, anOeffentlichenVerkehrsflaechen,
	       gelaendeneigungMin, inSchutzzone, plangebietAllgemein,
	       planzeichenBBID, struktureinheitBebaubar, arkadeHoehe,
	       arkadeLaenge, durchfahrtBreite, durchfahrtHoehe,
	       durchgangBreite, durchgangHoehe, laubengangHoehe,
	       laubengangLaenge, ausnahmePruefungErforderlich, nA,
	       strittigeBedeutung,
	       weitereBestimmungPruefungErforderlich,
	       zuVorherigemSatzGehoerig,
	       bbAusnuetzbarkeitWidmungskategorieGefoerderterWohnbau,
	       unzulaessigBueroGeschaeftsgebaeude,
	       verbotAufenthaltsraum, verbotWohnung, widmungID,
	       widmungErsteEbene, widmungZweiteEbene,
	       widmungZweiteEbeneBezugHoehe, widmungDritteEbene,
	       widmungDritteEbeneBezugHoehe,
	       anlageZumEinstellenVorhanden,
	       stellplatzImNiveauZulaessig, stellplatzMax,
	       stellplatzregulativUmfangMaximumAbsolut,
	       stellplatzregulativUmfangMaximumRelativ,
	       stellplatzregulativUmfangMinimumRelativ,
	       stellplatzregulativVorhanden, gehsteigbreiteMin,
	       strassenbreiteMax, strassenbreiteMin,
	       strassenbreiteVonBis, widmungErsteEbeneBezugHoehe,
	       bbAusnuetzbarkeitVolumenBaumasse,
	       bbAusnuetzbarkeitVolumenBaumasseRelativ,
	       bbAusnuetzbarkeitVolumenRelativ,
	       umbaubarerRaumBauplatzMax, umbaubarerRaumGebaeudeMax,
	       umbaubarerRaumGebaeudeteilMax,
	       vorstehendeBauelementeAusladungMax]).

/* measuretriple /3
   for implementing reasoning with measures, min_measures,
   max_measures, using arbitrary names.
   Naming scheme: measuretriple(Measure, Min_Measure, Max_Measure).
   Example: measuretriple(hoehe, hoeheMin, hoeheMax)
*/
measuretriple(anteilBaumbepflanzungGenau,anteilBaumbepflanzung,anteilBaumbepflanzungMax).
measuretriple(technischeAufbautenHoeheGenau, technischeAufbautenHoeheMin,
	      technischeAufbautenHoeheMax).
measuretriple(abschlussDachGenau,abschlussDachMin,abschlussDachMax).
measuretriple(anteilDachbegruenungGenau,anteilDachbegruenung,anteilDachbegruenungMax).
measuretriple(bbDachneigungGenau,bbDachneigungMin,bbDachneigungMax).
measuretriple(dachflaecheGenau,dachflaecheMin,dachflaecheMax).
measuretriple(dachneigungGenau,dachneigungMin,dachneigungMax).
measuretriple(einfriedungHoeheGesamtGenau,einfriedungHoeheGesamtMin,einfriedungHoeheGesamt).
measuretriple(einfriedungHoeheSockelGenau,einfriedungHoeheSockelMin,einfriedungHoeheSockel).
measuretriple(bauplatzUnterirdischeBebauungGenau,bauplatzUnterirdischeBebauungMin,bauplatzUnterirdischeBebauungMax).
measuretriple(bbAusnuetzbarkeitFlaecheBGFGenau,bbAusnuetzbarkeitFlaecheMin,bbAusnuetzbarkeitFlaecheBGF).
measuretriple(bbAusnuetzbarkeitFlaecheBGFRelativGenau,bbAusnuetzbarkeitFlaecheBGFRelativMin,bbAusnuetzbarkeitFlaecheBGFRelativ).
measuretriple(bbAusnuetzbarkeitFlaecheGrundflaechenbezugGenau,bbAusnuetzbarkeitFlaecheGrundflaechenbezugMin,bbAusnuetzbarkeitFlaecheGrundflaechenbezug).
measuretriple(bbAusnuetzbarkeitFlaecheGrundflaechenbezugRelativGenau,bbAusnuetzbarkeitFlaecheGrundflaechenbezugRelativMin,bbAusnuetzbarkeitFlaecheGrundflaechenbezugRelativ).
measuretriple(bbAusnuetzbarkeitFlaecheNutzflaecheGenau,bbAusnuetzbarkeitFlaecheNutzflaecheMin,bbAusnuetzbarkeitFlaecheNutzflaeche).
measuretriple(bbAusnuetzbarkeitFlaecheNutzflaecheRelativGenau,bbAusnuetzbarkeitFlaecheNutzflaecheRelativMin,bbAusnuetzbarkeitFlaecheNutzflaecheRelativ).
measuretriple(bbAusnuetzbarkeitFlaecheWohnnutzflaecheGenau,bbAusnuetzbarkeitFlaecheWohnnutzflaecheMin,bbAusnuetzbarkeitFlaecheWohnnutzflaeche).
measuretriple(bbAusnuetzbarkeitFlaecheWohnnutzflaecheRelativGenau,bbAusnuetzbarkeitFlaecheWohnnutzflaecheRelativMin,bbAusnuetzbarkeitFlaecheWohnnutzflaecheRelativ).
measuretriple(bbBebaubareFlaecheAbgegrenztGenau,bbBebaubareFlaecheAbgegrenztMin,bbBebaubareFlaecheAbgegrenzt).
measuretriple(bbBebaubareFlaecheGesamterBauplatzGenau,bbBebaubareFlaecheGesamterBauplatzMin,bbBebaubareFlaecheGesamterBauplatz).
measuretriple(bbBebaubareFlaecheJeBauplatzGenau,bbBebaubareFlaecheJeBauplatzMin,bbBebaubareFlaecheJeBauplatzMax).
measuretriple(bbBebaubareFlaecheJeGebaeudeGenau,bbBebaubareFlaecheJeGebaeudeMin,bbBebaubareFlaecheJeGebaeude).
measuretriple(bbBebaubareFlaechefuerNebengebaeudeJeBauplatzGenau,bbBebaubareFlaechefuerNebengebaeudeJeBauplatzMin,bbBebaubareFlaechefuerNebengebaeudeJeBauplatzMax).
measuretriple(bbBebaubareFlaechefuerNebengebaeudeJeBaulosGenau,bbBebaubareFlaechefuerNebengebaeudeJeBaulosMin,bbBebaubareFlaechefuerNebengebaeudeJeBaulosMax).
measuretriple(anzahlGeschosseOberirdischGenau,anzahlGeschosseOberirdischMin,maxAnzahlGeschosseOberirdisch).
measuretriple(anzahlGeschosseOberirdischDachgeschossGenau,anzahlGeschosseOberirdischDachgeschossMin,maxAnzahlGeschosseOberirdischDachgeschoss).
measuretriple(anzahlGeschosseOberirdischOhneDachgeschossGenau,anzahlGeschosseOberirdischOhneDachgeschossMin,maxAnzahlGeschosseOberirdischOhneDachgeschoss).
measuretriple(geschossanzahlGenau,geschossanzahlMin,zulaessigeGeschossanzahl).
measuretriple(einkaufszentrumFlaecheGenau,einkaufszentrumFlaecheMin,einkaufszentrumMaxFlaeche).
measuretriple(grossbauvorhabenFlaecheGenau,grossbauvorhabenFlaecheMin,grossbauvorhabenMaxFlaeche).
measuretriple(anschlussGebaeudeAnGelaende,anschlussGebaeudeAnGelaendeMin,anschlussGebaeudeAnGelaendeMax).
measuretriple(bbBauklasseGenau,bbBauklasseMinimum,bbBauklasseMaximum).
measuretriple(fbokWohnungenGenau,fbokMinimumWohnungen,fbokWohnungenMax).
measuretriple(gebaeudeHoeheGenau,gebaeudeHoeheMin,gebaeudeHoeheMax).
measuretriple(hoeheWohngebaeudeGenau,hoeheWohngebaeudeMin,maxHoeheWohngebaeude).
measuretriple(raumhoeheEGGenau,mindestraumhoeheEG,raumhoeheEGMax).
measuretriple(gelaendeneigungGenau,gelaendeneigungMin,gelaendeneigungMax).
measuretriple(arkadeHoeheGenau,arkadeHoehe,arkadeHoeheMax).
measuretriple(arkadeLaengeGenau,arkadeLaenge,arkadeLaengeMax).
% BLARGH: got to here.
measuretriple(durchfahrtBreiteGenau,durchfahrtBreite,durchfahrtBreiteMax).
measuretriple(durchfahrtHoeheGenau,durchfahrtHoehe,durchfahrtHoeheMax).
measuretriple(durchgangBreiteGenau,durchgangBreite,durchgangBreiteMax).
measuretriple(durchgangHoeheGenau,durchgangHoehe,durchgangHoeheMax).
measuretriple(laubengangHoeheGenau,laubengangHoehe,laubengangHoeheMax).
measuretriple(laubengangLaengeGenau,laubengangLaenge,laubengangLaengeMax).
measuretriple(bbAusnuetzbarkeitWidmungskategorieGefoerderterWohnbauGenau,bbAusnuetzbarkeitWidmungskategorieGefoerderterWohnbau,bbAusnuetzbarkeitWidmungskategorieGefoerderterWohnbauMax).
measuretriple(stellplatzGenau,stellplatzMin,stellplatzMax).
measuretriple(stellplatzregulativUmfangAbsolutGenau,stellplatzregulativUmfangAbsolutMin,stellplatzregulativUmfangMaximumAbsolut).
measuretriple(stellplatzregulativUmfangRelativGenau,stellplatzregulativUmfangMinimumRelativ,stellplatzregulativUmfangMaximumRelativ).
measuretriple(gehsteigbreiteGenau,gehsteigbreiteMin,gehsteigbreiteMax).
measuretriple(strassenbreiteGenau,strassenbreiteMin,strassenbreiteMax).
measuretriple(bbAusnuetzbarkeitVolumenBaumasseGenau,bbAusnuetzbarkeitVolumenBaumasseMin,bbAusnuetzbarkeitVolumenBaumasse).
measuretriple(bbAusnuetzbarkeitVolumenBaumasseRelativGenau,bbAusnuetzbarkeitVolumenBaumasseRelativMin,bbAusnuetzbarkeitVolumenBaumasseRelativ).
measuretriple(bbAusnuetzbarkeitVolumenRelativGenau,bbAusnuetzbarkeitVolumenRelativMin,bbAusnuetzbarkeitVolumenRelativ).
measuretriple(umbaubarerRaumBauplatzGenau,umbaubarerRaumBauplatzMin,umbaubarerRaumBauplatzMax).
measuretriple(umbaubarerRaumGebaeudeGenau,umbaubarerRaumGebaeudeMin,umbaubarerRaumGebaeudeMax).
measuretriple(umbaubarerRaumGebaeudeteilGenau,umbaubarerRaumGebaeudeteilMin,umbaubarerRaumGebaeudeteilMax).
measuretriple(vorstehendeBauelementeAusladungGenau,vorstehendeBauelementeAusladungMin,vorstehendeBauelementeAusladungMax).
/*
measuretriple(,,).
*/
