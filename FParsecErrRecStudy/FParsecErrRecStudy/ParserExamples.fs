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
let pEnd = skipString "end"

let charChoice = choice [a;b;c] .>> spaces 
// original parser
// let charSequence = sepBy charChoice comma |>> Ast.Sequence 
// modifications adding error recovery to charSequence:
let charChoiceBreakCondition = skipUntilLookaheadSeparatorFail comma rightBrace
let charChoiceErrRec = emitDiagnostics ad charChoiceBreakCondition "charChoice a|b|c expected"
let charSequence = sepBy (charChoice <|> charChoiceErrRec) comma |>> Ast.Sequence  // injection of charChoiceErrRec as an alternative to charChoice

// original parser
// let runSequence = sepBy runBlock semicolon |>> Ast.RunSequence 

// modifications after first adding error of recovery to runSequence:
// let runBlockBreakCondition = skipUntilLookaheadSeparatorFail semicolon pEnd
// let runBlockErrRec = emitDiagnostics ad runBlockBreakCondition "run block expected"
// let runBlock = (pRun >>. leftBrace >>. charSequence) .>> rightBrace |>> Ast.Run
// let runSequence = sepBy (runBlock <|> runBlockErrRec) semicolon .>> spaces |>> Ast.RunSequence // injection of runBlockErrRec as an alternative to runBlock 

// modifications adding error recovery to runBlock:
let runBlockBreakCondition = skipUntilLookaheadSeparatorFail semicolon pEnd
let runBlockErrRec = emitDiagnostics ad runBlockBreakCondition "run block expected"
let runBlockAbc = pRun >>. abc leftBrace charSequence rightBrace "{" "charSequence" "}" ad |>> Ast.Run
let runSequence = sepBy (runBlockAbc <|> runBlockErrRec) semicolon .>> spaces |>> Ast.RunSequence  // replacement of (runBlock <|> runBlockErrRec) by (runBlockABC <|> runBlockErrRec) 

// original parser
// let beginEndBlock = pBegin >>. runSequence .>> pEnd .>> spaces |>> Ast.Block
// let blockSequence = (many1 beginEndBlock) .>> spaces

// modifications adding error recovery to beginEndBlock:
let beginEndBlockAbc = abc pBegin runSequence pEnd "begin" "runSequence" "end" ad |>> Ast.Block 
let blockSequence = sepEndBy1 beginEndBlockAbc spaces1 // replacement by the 'abc' version of beginEndBlock

let globalParser = spaces >>. blockSequence .>> spaces .>> eof |>> Ast.Ast 

