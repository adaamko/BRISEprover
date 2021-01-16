<html >
<head>
<script src="jquery.min.js"></script>
</head>
<body>
<script>
//$("pdf_viewer").visibility="hidden" ;
</script>

<b><?php
$code = $_POST['ncode'];
$code_escaped = escapeshellarg($code);
$facts = $_POST['facts'];
$facts_escaped = escapeshellarg($facts);
$srauta = $_POST['srauta'];
$srauta_escaped = escapeshellarg($srauta);
$relation = $_POST['relation'];
$relation_escaped = escapeshellarg($relation);
$operators = $_POST['operators'];
$operators_escaped = escapeshellarg($operators);
$inclusions = $_POST['inclusions'];
$inclusions_escaped = escapeshellarg($inclusions);
$conflicts = $_POST['conflicts'];
$conflicts_escaped = escapeshellarg($conflicts);
$nontrivialities = $_POST['nontrivialities'];
$nontrivialities_escaped = escapeshellarg($nontrivialities);
$outputformat = $_POST['sel_output'];
$reasoning = $_POST['sel_reasoning'];
$examples = implode(',', $_POST['examples']);
//$intarray = $_POST['interpretations'];
//$interpretations = implode(',', $_POST['interpretations']);
//$output = exec("timeout 30 ./exec.sh \"$code\" \"$facts\" \"$srauta\" \"$relation\" \"$operators\" \"$inclusions\" \"$conflicts\" \"$nontrivialities\" \"$outputformat\" ");
$output = exec("timeout 15 ./exec.sh $code_escaped $facts_escaped $srauta_escaped $relation_escaped $operators_escaped $inclusions_escaped $conflicts_escaped $nontrivialities_escaped \"$examples\" \"$reasoning\" \"$outputformat\" ");
//$output = exec("timeout 15 ./exec.sh $code_escaped $facts_escaped
//$srauta_escaped $relation_escaped $operators_escaped
//$inclusions_escaped $conflicts_escaped $nontrivialities_escaped
//\"$examples\" \"$reasoning\" \"$outputformat\" ");
?></b>   

<?php
//echo $examples;
//echo "timeout 15 ./exec.sh $code_escaped $facts_escaped $srauta_escaped $relation_escaped $operators_escaped $inclusions_escaped $conflicts_escaped $nontrivialities_escaped \"$examples\" \"$reasoning\" \"$outputformat\" ";
//echo $output;
//echo $outputformat;
//echo "timeout 15 ./exec.sh $code_escaped $facts_escaped
//$srauta_escaped $relation_escaped $operators_escaped
//$inclusions_escaped $conflicts_escaped $nontrivialities_escaped
//\"$examples\" \"$reasoning\" \"$outputformat\" ";
//echo $interpretations;
//echo "timeout 30 ./exec.sh \"$code\" \"$facts\" \"$srauta\" \"$interpretations\" ";
//echo "timeout 30 ./exec.sh \"$code\" \"$facts\" \"$srauta\" \"$relation\" \"$operators\" \"$inclusions\" \"$conflicts\" \"$outputformat\" ";
//echo "timeout 30 ./exec.sh \"$code\" \"$facts\" \"$srauta\" \"$relation\" \"$operators\" \"$inclusions\" \"$conflicts\" \"$nontrivialities\" \"$examples\" ";
$output_arr = explode(";", $output);
if(empty($code)){
    echo "<center>ERROR: Please enter a formula to be checked!</center><br />";
}
else if(strcmp($output_arr[0], "0")==0){
    echo "got to point 1";
    echo "$output_arr[1]";
	echo "<center>These seems to have been a syntax problem - Please check your input!</center>";
}
else{
	if(strcmp($output_arr[0],"1")==0){
		echo "<center>The prover has found a derivation, but it is too large for TeX to handle.<br /> Please download the .tex file containing the derivation <a href=\"" . $output_arr[1] . "\">here</a>. You will also need the file <a href=\"output/header.sty\">header.sty</a>.</center>";
	}
	else{ 	
                  if(strcmp($output_arr[0],"3")==0){
                           echo "<center>Not derivable.</center>";
                  }
                  else{if(strcmp($outputformat, "explanation")==0){
                           readfile($output_arr[1]);
                       }
                      else{echo "<center><object data=\"" . $output_arr[1] . "\" type=\"application/pdf\" width=\"100%\" height=\"100%\"> Please download the PDF  <a href=\"" . $output_arr[1] . "\">here</a>.</object></center> ";
                      }
                  }
    }
}
?>

</body>
</html>

<!-- echo "<center> Command for script: exec(./exec.sh \"$code\" $logic $parameter ) . Path to PDF:  <a href=\"$output\">here</a>.</center> "; -->


<!--    if (preg_match('/(\.txt$)/' $output)){
      echo "No PDF generated";
   }
-->


<!-- if(strpos($output, '.txt')){
      echo "<center>No PDF generated - probably the derivation is too large for TeX or there was a syntax error.<br />Please download the output of the prover <a href=\"$output\">here</a></center>";
   } else{
 echo "<center><object data=\"$output\" type=\"application/pdf\" width=\"100%\" height=\"100%\"> Please download the PDF  <a href=\"$output\">here</a>.</object></center> ";}
-->

