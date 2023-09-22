open FParsec
open ErrRecovery
open ParserExamples


let firstInput1 = "begin run {a};run {a, b, ;run{a, b} end begin run{a,b,c}; run{a,b} end "
//let firstInput1 = "begin run {a,b,c};run {a} end "
//let firstInput1 = "begin run {a,b,c} end "
//let firstInput1 = "begin run {a} end "

//let firstInput1 = "begin run {a,b,d,a, };run{a, b,c} end begin run{a,b,c}; run{a,b} end"

printf "\n%s\n%O\n" firstInput1 (tryParse globalParser "expected at least one begin end block" ad firstInput1)
ad.PrintDiagnostics


ad.Clear()
let input3 = "{ } "
let result3 = run (abc leftBrace charSequence rightBrace "{" "charSequence" "}" ad) input3
printf "%O\n" result3 
ad.PrintDiagnostics

