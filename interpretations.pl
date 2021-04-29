/* 
   For finding alternative interpretations and scoring them.
   So far the implemented alternatives are:
    - for(int0): for(a,b) as for(a,b)
    - for(int1): for(a,b) as obl(neg a, b)
    - for(int2): for(a,b) as obl(a, neg b)
    - for(int3): for(a,b) as pero(neg a, b)
    - obl(int0): obl(a,b) as obl(a,b)
    - obl(int1): obl(a,b) as rec(a,b)
    - obl(int2): obl(a,b) as perf(a,b)
*/
  :- use_module(library(lists)).

/* alternatives /2
   takes Facts and Srauta, produces the scored alternatives and prints
   everything on the screen
*/
/* old version, still used in the online prover producing the latex
 * file
*/
alternativesWrite(StreamName,Ints,Facts,Srauta, Relation, Input) :-
    alternativeN(Ints, Facts, Srauta, Relation, Input, ScoredList),
    nl,write('possible interpretations of the deontic assumptions:'),nl,nl,
    maplist(ppScoredList,ScoredList),
    open(StreamName, append, Stream2),
    write(Stream2,'Possible alternative interpretations of the deontic assumptions: \\medskip'),nl(Stream2),nl(Stream2),
    maplist(ppwriteScoredList(Stream2),ScoredList),
    close(Stream2),!.
/* alternative_interpretations
 * New version for printing the alternative intepretations using DCGs
*/
alternative_interpretations(StreamName,Ints,Facts,Srauta, Relation, Input) :-
    alternativeN(Ints, Facts, Srauta, Relation, Input, ScoredList),
    nl,write('possible interpretations of the deontic assumptions:'),nl,nl,
    maplist(ppScoredList,ScoredList),
    open(StreamName, append, Stream2),
    write(Stream2,'Possible alternative interpretations of the deontic assumptions: \\medskip'),nl(Stream2),nl(Stream2),
    maplist(ppwriteScoredList(Stream2),ScoredList),
    close(Stream2),!.

/* alternative /3 
   takes Facts, Srauta and Relation lists and produces
   a list of alternative interpretations with "scores" in the form of
   tuples scInt(SrautaAlt,Score) where SrautaAlt is the alternative
   interpretation, and Score is the resulting Score
*/

alternativeN(IntIn,Facts,Srauta,Relation,Input,ScoredVikInt) :-
    list_to_set(IntIn,Ints),
    splitProh(Srauta, split(Proh,Rest1)), % split off prohibitions
    splitObl(Rest1,split(Obl,Rest2)), % split off obligations
    calculateProhLists(Ints,Proh,ChangedPartProhList),
    calculateOblLists(Ints,Obl,ChangedPartOblList),
    findall(tuple(FX,OX), (member(FX, ChangedPartProhList),
			   member(OX, ChangedPartOblList)),
	    CombinationList),
    /* Now we have a list of tuples tuple( [alt(for(int1),P1),...],
     * [alt(obl(int1),Q1,...] )
    */
    maplist(combineScoreLists(Rest2),CombinationList,CombinationList2)
    ,
    maplist(addVikalpaScore(Facts,Relation),CombinationList2,
	    Combination_List3),
    maplist(add_input_derivability(Facts,Relation,Input),
	    Combination_List3,ScoredVikInt).


/* calculateProhLists */
calculateProhLists(Ints,Proh,ChangedPartProhList) :-
    include(isFor,Ints,IntsFor), % split off Prohibition interpretations
    length(IntsFor,NFor), % get number of prohibition interpretations
    findall(PartProh, partitions_parameterised(Proh,NFor,PartProh),
	    PartProhList), % find all partitions of prohibitions
    maplist(altify(IntsFor),PartProhList,AltPartProhList), % combine
                                                           % partition
                                                           % with
                                                           % interpretations
    maplist(changeListN,AltPartProhList,ChangedPartProhList). % change
                                                              % according
                                                              % to
                                                              % interpretation 
    
/* calculateOblList */
calculateOblLists(Ints,Obl,ChangedPartOblList) :-
    include(isObl,Ints,IntsObl), % split off obl interpretations
    length(IntsObl,NObl), % get number of obl interpretations
    findall(PartObl, partitions_parameterised(Obl,NObl,PartObl),
	    PartOblList), % find all partitions of obligations
    maplist(altify(IntsObl),PartOblList,AltPartOblList), % same for
                                                         % obl
    maplist(changeListN,AltPartOblList,ChangedPartOblList). % same for obl


/* splitProh /2: 
 * splits Srauta assumptions into prohibition formulae and the rest.
*/
splitProh(Srauta, split(Proh,Rest)) :-
    partition( isFor, Srauta, ProhList, Rest),
    list_to_set(ProhList,Proh).

isFor(Term) :-
    Term =.. [for|_].
isFor(_:Term) :-
    Term =.. [for|_].

/* splitObl /2:
 * splits Srauta assumptions into obligation formulae and the rest.
*/
splitObl(Srauta,split(Obl,Rest)) :-
    partition( isObl, Srauta, OblList, Rest),
    list_to_set(OblList,Obl).

isObl(Term) :-
    Term =.. [obl|_].
isObl(_:Term) :-
    Term =.. [obl|_].

/* partitions_parameterised(List,N,Part): true if Part is a partition
 * of List into N subsets.
*/
partitions_parameterised(List,1,[List]).
partitions_parameterised(List, N, [P1|Part]) :-
    N > 1,
    M is N-1,
    part(List,P1,P2),
    partitions_parameterised(P2,M,Part).


/* part /3:
   succeeds if List is the union of First and Second
*/
part([],[],[]).
part([X|Rest],[X|First],Second) :-
    part(Rest,First,Second).
part([X|Rest],First,[X|Second]) :-
    part(Rest,First,Second).
    

/* altify(IntList,Partition,AltPartition):
 * zips together IntList and Partition into a list of alt(Int,Part).
*/
altify([],[],[]).
altify([Int|Ints],[Part|Parts],[alt(Int,Part)|Rest]) :-
    altify(Ints,Parts,Rest).


/* changeListN /2:
 * takes a list [alt(Type,List),...] where Type is the type of the new
 * interpretation, and List is a list of formulae, and changes it to
 * [alt(Type,NewList),...], where all the formulae in NewList are
 * changed according to the Type.
*/
changeListN([],[]).
changeListN([alt(Type,List)|Rest],[alt(Type,NewList)|NewRest]) :-
    maplist(changeN(Type),List,NewList),
    changeListN(Rest,NewRest).


/* changeN(Int,Fml,NewFml):
 * reinterprets Fml according to Int, resulting in NewFml
 * ADD NEW INTERPRETATIONS HERE.
 * New interpretations: have the form Op(Code), where Op is the
 * operator for / obl, and Code is the code of the interpretation.
*/
%changeN(other,X,X).
changeN(for(int0),X,X).
changeN(for(int1),for(X,Y),obl(neg X,Y)).
changeN(for(int1),N:for(X,Y),N:obl(neg X,Y)).
changeN(for(int2),for(X,Y),obl(X, neg Y)).
changeN(for(int2),N:for(X,Y),N:obl(X, neg Y)).
changeN(for(int3),for(X,Y),pero(neg X, Y)).
changeN(for(int3),N:for(X,Y),N:pero(neg X, Y)).
changeN(obl(int0),X,X).
changeN(obl(int1),obl(X,Y),rec(X,Y)).
changeN(obl(int1),N:obl(X,Y),N:rec(X,Y)).
changeN(obl(int2),obl(X,Y),perf(X,Y)).
changeN(obl(int2),N:obl(X,Y),N:perf(X,Y)).


/* changeProh /2:
   takes list of tuples splitProh(X,Y) and changes every prohibition
   in Y to a corresponding negative obligation.
*/
changeProh(splitProh(X,Y),splitProh(X,Z)) :-
    maplist(change, Y,Z).
change(for(X,Y), obl(X,neg Y)).
change(N:for(X,Y), N:obl(X,neg Y)).
change(for(X), obl(neg X)).
change(N:for(X), N:obl(neg X)).
change(X,X) :-
    \+ X =.. [for|_].

changeObl(splitObl(X,Y), splitObl(X,Z)) :-
    maplist(changeO,Y,Z).
changeO(obl(X,Y), rec(X,Y)).
changeO(X,X) :-
    \+ X =.. [obl|_].


/* calculateScoreNew(AltList,scoreList(List,Scores)) /2
 * takes a list of alternative interpretations of the form
 * alt(Int,AltList), combines all the AltLists into List, and
 * calculates the list Scores of scores scr(Int,N) for each
 * interpretation.
*/
calculateScoreNew([],scoreList([],[])).
calculateScoreNew([alt(Int,List)|Rest],scoreList(ListNew,[scr(Int,N)|ScoreList])) :-
    length(List,N),
    calculateScoreNew(Rest,scoreList(ListOld,ScoreList)),
    append(List,ListOld,ListNew).

combineScoreLists(Rest,tuple(ProhList,OblList),scoreList(List,Scores))
:-
    calculateScoreNew(ProhList,scoreList(Proh,ProhScore)),
    calculateScoreNew(OblList,scoreList(Obl,OblScore)),
    append(Obl,Proh,Tmp),
    append(Rest,Tmp,List),
    append(ProhScore,OblScore,Scores).


/* calculateScore /2:
   calculates the score of a splitProhList and returns the "scored
   list"
   The score is a list containing nat1(N) for the number of "natural
   1" things, and nat2(N) for the number of "natural 2" things.
*/
calculateScore(splitProh(X,Y),scoreList(List,Score)) :-
    length(X,LengthX),
    length(Y,LengthY),
    Score = [nat1(LengthX),nat2(LengthY)],
    union(X,Y,List).

calculateOblScore(splitObl(X,Y),scoreOblList(List,Score)) :-
    length(X,LengthX),
    length(Y,LengthY),
    Score = [natObl1(LengthX),natObl2(LengthY)],
    union(X,Y,List).

calculateScoreN(List,ScoredList) :-
    maplist(List,score,ScoredList).


/* score /2: change alt(Int,FmlList) to scr(Int,#FmlList).
*/
score(alt(X,Y),scr(X,Z)) :-
    length(Y,Z).


/* combineSplits /3:
   takes scoredProhList and scoredOblList and gives the combination.
*/
combineSplits(tuple(scoreList(ProhList,ProhScore),
	      scoreOblList(OblList,OblScore)),scoreList(Result,Score))
:-
    append(ProhList,OblList,Result),
    append(ProhScore,OblScore,Score).


/* addRest /3:
   adds the rest to a scored list
*/
addRest(Rest,scoreList(List,Score),scoreList(Result,Score)) :-
    append(Rest,List,Result).


/* vikalpaScore
   Calculates the number of vikalpas
*/
addVikalpaScore(Facts,Relation,scoreList(List,Score),scoreList(List,[vik(VkScore,Vikalpas)|Score])) :-
    vikalpaCheck(Facts,List,Relation,Vikalpas),
    length(Vikalpas,VkScore).


/* add_input_derivability
 * adds inp(T) to a scoreList, where T is the result of trying to
 * derive the input using the interpretation given by
 * scoreList(List,Score).
*/
add_input_derivability(Facts, Relation, Input, scoreList(List,Score),
		       scoreList(List, [inp(T)|Score])) :-
    (prove([],asmp(Facts,List,Relation), [Input], T)
    ;
     nonderivable_statement(T)),!.


/* pretty printing */

/* ppScoredList:
   prints a scored interpretation on the screen
*/
ppScoredList(scoreList(List,Score)) :-
    write('Interpretation: '),
    write(List),nl,
    tab(2),
    write('number of underivable deontic assumptions: '),
    member(vik(N,_),Score),
    write(N),nl,tab(2),
    ppIntScore(for(int0),Score),
    ppIntScore(for(int1),Score),
    ppIntScore(for(int2),Score),
    ppIntScore(for(int3),Score),
    ppIntScore(obl(int0),Score),
    ppIntScore(obl(int1),Score),
    ppIntScore(obl(int2),Score),
    nl.


/* ppInt: pretty print the intepretation */
ppInt(for(int0)) :-
    write('Most natural prohibitions (as for(.,.)): ').
ppInt(for(int1)) :-
    write('Next most natural prohibitions (as obl(neg .,.)): ').
ppInt(for(int2)) :-
    write('Next most natural prohibitions (as obl(.,neg .)): ').
ppInt(for(int3)) :-
    write('Least natural prohibitions (as pero(neg .,.)): ').
ppInt(obl(int0)) :-
    write('Most natural obligations (as obl(.,.)): ').
ppInt(obl(int1)) :-
    write('Next most natural obligations (as rec(.,.)): ').
ppInt(obl(int2)) :-
    write('Least natural obligations (as perf(.,.)): ').


ppIntScore(_,[]).
ppIntScore(Int,[scr(Int,N)|_]) :-
    ppInt(Int),
    write(N),nl,tab(2),!.
ppIntScore(Int,[_|Rest]) :-
    ppIntScore(Int,Rest).


/* for latexing: */
ppwriteScoredList(Stream,scoreList(List,Score)) :-
    write(Stream,'\\noindent Interpretation: $'),
    ppwriteFList(Stream,List),write(Stream,'\\;$'),nl(Stream),
% Add this for adding information about derivability of the input to
% the various interpretations.
    nl(Stream),
    (member(inp(nonderivable),Score),
     write(Stream,'\\noindent Status of input under this interpretation:
   nonderivable.'),
     nl(Stream)
    ;
    member(inp(T),Score),
    T \== nonderivable,
    write(Stream,'\\noindent Status of input under this interpretation:
   derivable.'),
    nl(Stream),nl(Stream),
    ppwrite(Stream,T)),
    write(Stream,'\\begin{itemize}'),nl(Stream),
    write(Stream,'\\item number of underivable deontic assumptions: '),
    member(vik(N,Vikalpas),Score),
    write(Stream,N),nl(Stream),
    write(Stream,'\\\\'),nl(Stream),
    write(Stream,'list of underivable deontic assumptions: $'),
    ppwriteFList(Stream,Vikalpas),write(Stream,'\\;$'),nl(Stream),
    ppwriteIntScore(Stream,for(int0),Score),
    ppwriteIntScore(Stream,for(int1),Score),
    ppwriteIntScore(Stream,for(int2),Score),
    ppwriteIntScore(Stream,for(int3),Score),
    ppwriteIntScore(Stream,obl(int0),Score),
    ppwriteIntScore(Stream,obl(int1),Score),
    ppwriteIntScore(Stream,obl(int2),Score),
    nl(Stream),
    write(Stream,'\\end{itemize} \\medskip'),
    nl(Stream), nl(Stream).


/* ppwriteInt: pretty print the intepretation in latex*/
ppwriteInt(Stream,for(int0)) :-
    write(Stream,'\\item Most natural prohibitions (as $\\for(.,.)$): ').
ppwriteInt(Stream,for(int1)) :-
    write(Stream,'\\item Next most natural prohibitions (as $\\obl(\\neg .,.)$): ').
ppwriteInt(Stream,for(int2)) :-
    write(Stream,'\\item Next most natural prohibitions (as $\\obl(.,\\neg .)$): ').
ppwriteInt(Stream,for(int3)) :-
    write(Stream,'\\item Least natural prohibitions (as $\\pero(\\neg .,.)$): ').
ppwriteInt(Stream,obl(int0)) :-
    write(Stream,'\\item Most natural obligations (as $\\obl(.,.)$): ').
ppwriteInt(Stream,obl(int1)) :-
    write(Stream,'\\item Next most natural obligations (as $\\rec(.,.)$): ').
ppwriteInt(Stream,obl(int2)) :-
    write(Stream,'\\item Least natural obligations (as $\\perf(.,.)$): ').


ppwriteIntScore(_,_,[]).
ppwriteIntScore(Stream,Int,[scr(Int,N)|_]) :-
    ppwriteInt(Stream,Int),
    write(Stream,N),nl(Stream),tab(Stream,2),!.
ppwriteIntScore(Stream,Int,[_|Rest]) :-
    ppwriteIntScore(Stream,Int,Rest).
