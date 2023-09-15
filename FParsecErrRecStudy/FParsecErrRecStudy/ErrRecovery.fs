module ErrRecovery
open System.Collections.Generic
open FParsec
open TheParserTypes

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
        preturn ()
    escapeParser |>> SyntaxNode.Escape

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