module ParserExamples
open FParsec
open TheParserTypes
open ErrRecovery


//begin run {a,b,c };run{a, b,c} end beg in run{a,b,c}; run{a,b} end 

// Parsers

let a = skipChar 'a' .>> spaces >>% SyntaxNode.A
let b = skipChar 'b' .>> spaces >>% SyntaxNode.B
let c = skipChar 'c' .>> spaces >>% SyntaxNode.C
let comma = skipChar ',' .>> spaces

// original parser
// let charChoice = choice [a;b;c] .>> spaces
// modifications adding error recovery to charChoice:
let charChoiceBreakCondition = skipUntilLookaheadSeparator comma
let charChoiceErrRec = emitDiagnostics ad charChoiceBreakCondition "charChoice a|b|c expected" 
let charChoice = choice [a;b;c;charChoiceErrRec] .>> spaces

let charSequence = sepBy charChoice comma |>> SyntaxNode.Sequence 
let leftBrace: Parser<_, unit> = skipChar '{' >>. spaces
let rightBrace: Parser<_, unit>  = skipChar '}' >>. spaces
let runBlock = (skipString "run" >>. spaces >>. leftBrace >>. spaces >>. charSequence .>> spaces) .>> rightBrace .>> spaces |>> SyntaxNode.Run
let semicolon = skipChar ';' .>> spaces
let runSequence = sepBy runBlock semicolon |>> SyntaxNode.RunSequence
let pBegin = skipString "begin" >>. spaces 
let pEnd = skipString "end" >>. spaces 
let beginEndBlock = pBegin >>. runSequence .>> pEnd .>> spaces |>> SyntaxNode.Block
let blockSequence = many1 beginEndBlock
let globalParser = blockSequence .>> eof |>> SyntaxNode.Ast

