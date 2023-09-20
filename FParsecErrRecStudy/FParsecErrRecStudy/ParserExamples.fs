module ParserExamples
open FParsec
open TheParserTypes
open ErrRecovery


//begin run {a,b,c };run{a, b,c} end beg in run{a,b,c}; run{a,b} end 

// Parsers

let a = skipChar 'a' .>> spaces >>% Ast.A
let b = skipChar 'b' .>> spaces >>% Ast.B
let c = skipChar 'c' .>> spaces >>% Ast.C
let leftBrace: Parser<_, unit> = skipChar '{' >>. spaces
let rightBrace: Parser<_, unit>  = skipChar '}' >>. spaces
let semicolon = skipChar ';' .>> spaces 
let comma = skipChar ',' .>> spaces 
let pBegin = skipString "begin" >>. spaces 
let pRun = skipString "run" >>. spaces 
let pEnd = skipString "end" >>. spaces 

let charChoice = choice [a;b;c] .>> spaces
// original parser
// let charSequence = sepBy charChoice comma |>> Ast.Sequence 
// modifications adding error recovery to charSequence:
let charChoiceBreakCondition = skipUntilLookaheadSeparatorFail comma rightBrace 
let charChoiceErrRec = emitDiagnostics ad charChoiceBreakCondition "charChoice a|b|c expected" 
let charSequence = sepBy (charChoice <|> charChoiceErrRec) comma |>> Ast.Sequence // injection of charChoiceErrRec in choice

let runBlock = (pRun >>. leftBrace >>. charSequence) .>> rightBrace |>> Ast.Run
// original parser
// let runSequence = sepBy runBlock semicolon |>> Ast.RunSequence 
// modifications adding error recovery to runSequence:
let runBlockMissingSomething = pRun >>. abc leftBrace charSequence rightBrace "{" "charSequence" "}" ad |>> Ast.Run
let runBlockBreakCondition = skipUntilLookaheadSeparatorFail semicolon pEnd
let runBlockErrRec = emitDiagnostics ad runBlockBreakCondition "run block expected"
let tryRunBlock = choice [runBlockMissingSomething ; runBlockErrRec]
let runSequence = sepBy tryRunBlock semicolon .>> spaces |>> Ast.RunSequence // injection of choice between runBlock and runBlockErrRec

let beginEndBlock = pBegin >>. runSequence .>> pEnd .>> spaces |>> Ast.Block
let blockSequence = (many1 beginEndBlock) .>> spaces
let globalParser = spaces >>. blockSequence .>> eof |>> Ast.Ast

