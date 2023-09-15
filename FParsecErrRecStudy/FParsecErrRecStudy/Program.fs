open FParsec
open System.Collections.Generic

type SyntaxNode = 
    | Error
    | String of string
    | Int of int64
    | Escape of unit
    | ManyEitherOr of SyntaxNode list

type DiagnosticEmitter = Parser | Interpreter
type DiagnosticSeverity = Error | Warning | Severity | Hint 
type DiagnosticMessage = DiagnosticMessage of string
type Diagnostic = Diagnostic of DiagnosticEmitter * DiagnosticSeverity * Position * DiagnosticMessage
type Diagnostics () =
    let myList = new List<Diagnostic>()
    member this.List with get() = myList
    member this.AddDiagnostic d = myList.Add(d)
    member this.PrintDiagnostics = 
        for d in myList do printfn "%O" d
        printfn "%s" "\n------------------------------\n"
    member this.Clear = myList.Clear

let ad = Diagnostics() 
            


/// A helper function to emitDiagnostics in the current user state and position.
let emitDiagnostics (ad:Diagnostics) escapeParser msg = 
    let errorMsg = DiagnosticMessage msg
    getPosition >>= fun pos -> 
        let diagnostic = Diagnostic (DiagnosticEmitter.Parser, DiagnosticSeverity.Error,pos,errorMsg)
        ad.AddDiagnostic diagnostic
        escapeParser |>> SyntaxNode.Escape

// Parser

let intValue = pint64 .>> spaces |>> SyntaxNode.Int
let stringValue: Parser<SyntaxNode, unit>  = pstring "ab" .>> spaces |>> SyntaxNode.String
let escapeEitherOr = 
    emitDiagnostics ad (skipAnyOf " ") "expected EitherOr"

/// Performs many choices of eitherOr and escapes any errors occurring 
/// while being in the context of eitherOr.
let eitherOr = many (choice [ stringValue ; intValue ; escapeEitherOr ]) |>> SyntaxNode.ManyEitherOr

/// Global parser that wants to consume everything until and of file
let parser = eitherOr .>> eof

/// Emit any errors occurring in the globalParser
let tryParse globalParser input expectMessage (ad:Diagnostics) = 
    match run globalParser input with
    | Success(result, restInput, userState) -> 
        result 
    | Failure(errorMsg, restInput, userState) -> 
        let diagnosticMsg = DiagnosticMessage (expectMessage + " " + errorMsg)
        let diagnostic = Diagnostic (DiagnosticEmitter.Parser, DiagnosticSeverity.Error,restInput.Position,diagnosticMsg)
        ad.AddDiagnostic diagnostic
        SyntaxNode.Error

let input1 = "ab ab 11 d"
let input2 = "ab ab 11 "
let input3 = "abab11"

printf "\n%s\n%O\n" input1 (tryParse parser input1 "expected sequence of EitherOr" ad)
ad.PrintDiagnostics

ad.Clear()
printf "\n%s\n%O\n" input2 (tryParse parser input2 "expected sequence of EitherOr" ad)
ad.PrintDiagnostics

ad.Clear()
printf "\n%s\n%O\n" input3 (tryParse parser input3 "expected sequence of EitherOr" ad)
ad.PrintDiagnostics


