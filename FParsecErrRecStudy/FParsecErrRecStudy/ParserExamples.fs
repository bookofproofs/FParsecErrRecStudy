module ParserExamples
open FParsec
open TheParserTypes
open ErrRecovery


//begin run {a,b,c };run{a, b,c} end beg in run{a,b,c}; run{a,b} end 

// Parsers

let a = skipChar 'a' .>> spaces >>% SyntaxNode.A
let b = skipChar 'b' .>> spaces >>% SyntaxNode.B
let c = skipChar 'c' .>> spaces >>% SyntaxNode.C
let leftBrace: Parser<_, unit> = skipChar '{' >>. spaces
let rightBrace: Parser<_, unit>  = skipChar '}' >>. spaces
let semicolon = skipChar ';' .>> spaces
let comma = skipChar ',' .>> spaces
let pBegin = skipString "begin" >>. spaces 
let pRun = skipString "run" >>. spaces 
let pEnd = skipString "end" >>. spaces 

let charChoice = choice [a;b;c] .>> spaces
// original parser
// let charSequence = sepBy charChoice comma |>> SyntaxNode.Sequence 
// modifications adding error recovery to charSequence:
let charChoiceBreakCondition = skipUntilLookaheadSeparatorFail comma rightBrace 
let charChoiceErrRec = emitDiagnostics ad charChoiceBreakCondition "charChoice a|b|c expected" 
let charSequence = sepBy (charChoice <|> charChoiceErrRec) comma |>> SyntaxNode.Sequence // injection of charChoiceErrRec in choice

let runBlock = (pRun >>. leftBrace >>. spaces >>. charSequence .>> spaces) .>> rightBrace .>> spaces |>> SyntaxNode.Run
// original parser
// let runSequence = sepBy runBlock semicolon |>> SyntaxNode.RunSequence 
// modifications adding error recovery to runSequence:
let runBlockBreakCondition = skipUntilLookaheadSeparatorFail semicolon pEnd
let runBlockErrRec = emitDiagnostics ad runBlockBreakCondition "run block expected" 
let runSequence = sepBy (runBlock <|> runBlockErrRec) semicolon |>> SyntaxNode.RunSequence // injection of choice between runBlock and runBlockErrRec

let beginEndBlock = pBegin >>. runSequence .>> pEnd .>> spaces |>> SyntaxNode.Block
let blockSequence = many1 beginEndBlock
let globalParser = blockSequence .>> eof |>> SyntaxNode.Ast

