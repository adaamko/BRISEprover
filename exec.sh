#!/bin/bash 
#

tmp_file1=$(mktemp ./output/XXXXXXXXXX)
tmp_file2=$tmp_file1".tex"
tmp_file3=$tmp_file1".txt"
tmp_file4=$tmp_file1".html"

prolog_command="./iprove.out"
latex="/usr/bin/pdflatex"
# return_value=$tmp_file1".pdf"

#while [ "tmp_file1" != "bye" ]
#do
#    echo "loop... "
#done


if [[ -z  $1  ]] ;  then
	echo "Mode of use ./exec.sh goal" ;
	echo "For instance: ./exec.sh 'prv( (a -> b) or (b -> a), gd)'." ;
	echo "prove_online( $1 , [$2] , [$3] , [$4] , [$5] , [$6], [$7], [$8], [$9], '${10}', 'derivability', '$tmp_file2' )";
	exit;
fi

# strcmd="echo $1 | $prolog_command" ;
# echo $1 | $prolog_command > /dev/null 2 > /dev/null #2>&1 #| grep true
# echo $1 | $prolog_command | grep halt
if test "${11}" = "derivation"
   then
       echo "Got to step 1 for derivation!";
       swipl -q -g "prove_online( $1 , [$2] , [$3] , [$4] , [$5] , [$6], [$7], [$8], [$9], '${10}', 'derivability', '$tmp_file2' )" -t halt 'deonticProver.pl' &> $tmp_file3
else
    echo "got to step 1 for explanation!";
    swipl -q -g "explain_online( $1 , [$2] , [$3] , [$4] , [$5] , [$6], [$7], [$8], [$9], '${10}', 'derivability', '$tmp_file4' )" -t halt 'deonticProver.pl' &> $tmp_file3
fi    
# mv 'output/output.tex' $tmp_file1".tex"
if grep -q ERROR $tmp_file3; then
	echo "0;outputContainsERROR";
	exit;
fi

if test "${11}" = "derivation"
   then
       echo "got to step 2 for derivation";
       $latex -halt-on-error -output-directory ./output/ $tmp_file2 > /dev/null
       rm -f $tmp_file1".aux"
       #rm -f $tmp_file1".tex"
       rm -f $tmp_file1".log"
       #rm -f $tmp_file1".pdf"
       rm -f $tmp_file1



       if !( grep -q "end{document}" $tmp_file1".tex"); then # and txt contains input ok, then failed to prove
           if ( grep -q "input ok" $tmp_file1".txt"); then
               echo "3;failed";
               exit;
	   fi
	   echo "0;weirdLaTexError";
	   exit;
       fi

       if [ ! -e $tmp_file1".pdf" ]
       then
    	   return_value=$tmp_file1".tex";
    	   echo "1;$return_value";
       else
    	   return_value=$tmp_file1".pdf";
    	   echo "2;$return_value"
       fi

else
    echo "got to step 2 for explanation";
    rm -f $tmp_file1

    #if !( grep -q "end{document}" $tmp_file1".tex"); then # and txt contains input ok, then failed to prove
    #        if ( grep -q "input ok" $tmp_file1".txt"); then
    #            echo "3;failed";
    #            exit;
    #	fi
    #	echo "0;nothing";
    #	exit;
    #fi

    if [ ! -e $tmp_file1".html" ]
    then
    	echo "0;noHTMLfile";
	exit;
    else
    	return_value=$tmp_file1".html";
    	echo "2;$return_value"
    fi
fi
