module ParserExamples
open FParsec
open TheParserTypes
open ErrRecovery

// Parsers

let intValue = pint64 .>> spaces |>> SyntaxNode.Int
let stringValue = pstring "ab" .>> spaces |>> SyntaxNode.String

/// Performs many choices of eitherOr and escapes any errors occurring 
/// while being in the context of eitherOr.
let leftBrace: Parser<_, unit> = skipChar '{'
let rightBrace: Parser<_, unit>  = skipChar '}'

let eitherOrEscape = 
    emitDiagnostics ad (skipAnyOf " ") "expected EitherOr"
// let eitherOrEscape = (skipAnyOf " ") |>> SyntaxNode.Escape
let eitherOr = choice [ stringValue ; intValue ; eitherOrEscape ]
let eitherOrList = many eitherOr |>> SyntaxNode.ManyEitherOr

/// A first final parser that consumes everything until the end of file
let firstParserExample = eitherOrList .>> eof

let keyword: Parser<_, unit> = skipString "run" 

let namedBlockEscape = 
    emitDiagnostics ad (skipAnyOf " \t\n\r}") "expected namedBlock"

let namedBlock = (keyword >>. spaces >>. leftBrace >>. spaces >>. eitherOrList) .>> (spaces >>. rightBrace >>. spaces) |>> SyntaxNode.NamedBlock
let namedBlockList = many (spaces >>. (namedBlock <|> namedBlockEscape) .>> spaces) |>> SyntaxNode.NamedBlockList

let secondParserExample = namedBlockList .>> eof


