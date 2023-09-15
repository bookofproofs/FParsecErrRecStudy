open FParsec
open ErrRecovery
open ParserExamples


let firstInput1 = "ab ab 11 d"
let firstInput2 = "ab ab 11 "
let firstInput3 = "abab11"
let firstInput4 = "11"
let firstInput5 = "ab"

printf "\n%s\n%O\n" firstInput1 (tryParse firstParserExample firstInput1 "expected sequence of EitherOr" ad)
ad.PrintDiagnostics

ad.Clear()
printf "\n%s\n%O\n" firstInput2 (tryParse firstParserExample firstInput2 "expected sequence of EitherOr" ad)
ad.PrintDiagnostics

ad.Clear()
printf "\n%s\n%O\n" firstInput3 (tryParse firstParserExample firstInput3 "expected sequence of EitherOr" ad)
ad.PrintDiagnostics

ad.Clear()
printf "\n%s\n%O\n" firstInput4 (tryParse firstParserExample firstInput4 "expected sequence of EitherOr" ad)
ad.PrintDiagnostics

ad.Clear()
printf "\n%s\n%O\n" firstInput5 (tryParse firstParserExample firstInput5 "expected sequence of EitherOr" ad)
ad.PrintDiagnostics

let secondInput1 = "run{11} run{ab ab 11 d} run{ 11ab } "
let secondInput2 = "run{11} run{ab ab 11} run{ 11ab } "
let secondInput3 = "run{11} run{ab d 11 } run{ 11ab } "

ad.Clear()
printf "\n%s\n%O\n" secondInput1 (tryParse secondParserExample secondInput1 "expected sequence of named blocks" ad)
ad.PrintDiagnostics

ad.Clear()
printf "\n%s\n%O\n" secondInput2 (tryParse secondParserExample secondInput2 "expected sequence of named blocks" ad)
ad.PrintDiagnostics

ad.Clear()
printf "\n%s\n%O\n" secondInput3 (tryParse secondParserExample secondInput3 "expected sequence of named blocks" ad)
ad.PrintDiagnostics