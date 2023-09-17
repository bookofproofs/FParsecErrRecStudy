open FParsec
open ErrRecovery
open ParserExamples


let firstInput1 = "begin run {a,c,d ,a, };run{a, b} end begin run{a,b,c}; run{a,b} end "

printf "\n%s\n%O\n" firstInput1 (tryParse globalParser "expected at least one begin end block" ad firstInput1)
ad.PrintDiagnostics
