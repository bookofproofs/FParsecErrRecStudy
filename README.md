# FParsecErrRecStudy

A study of error recovery and how it could be done with FParsec.


## Goal of this study
To add error recovery to a more complex grammar that would recover from errors occurring in many contexts.
In particular, we would like to return at least a partial AST where our parser can succeed in error recovery and emit diagnostics depending on the context in which the error recovery did succeed.
Only a the case none of our context-related error recovery attempts succeed, our partial AST would be replaced by a single ERROR node, while still emitting diagnostics why this happened.

## Example grammar 
The grammar we want to parse is (in ebnf)

```
a = 'a' SPACES
b = 'b' SPACES
c = 'c' SPACES
leftBrace = '{' SPACES
rightBrace = '}' SPACES
semicolon = ';' SPACES
comma = ',' SPACES
pBegin = "begin" SPACES
pRun = "run" SPACES
pEnd = "end" SPACES

charChoice = a | b | c
charSequence = (charChoice (comma charChoice)*)?
runBlock = pRun leftBrace charSequence rightBrace
runSequence = (runBlock (semicolon runBlock)*)?
beginEndBlock = pBegin runSequence pEnd
blockSequence = beginEndBlock+
globalParser = blockSequence EOF

```

Thus, a syntactically correct productions of this grammar would be, for instance:

```
"begin run {a,b,a};run{a, b,c} end begin run{a,b,c}; run{a,b} end
"begin run {a,b,c};run {a} end "
"begin run {a,b,c} end "
"begin run {a} end "
```

Our grammar in FParsec that does not yet support error recovery whatsoever can be written as
```
type Ast = 
    | A 
    | B 
    | C 
    | Sequence of Ast list
    | Run of Ast 
    | RunSequence of Ast list
    | Block of Ast 
    | Ast of Ast list
    
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
let charSequence = sepBy charChoice comma |>> Ast.Sequence 
let runBlock = (pRun >>. leftBrace >>. spaces >>. charSequence .>> spaces) .>> rightBrace .>> spaces |>> Ast.Run
let runSequence = sepBy runBlock semicolon |>> Ast.RunSequence 
let beginEndBlock = pBegin >>. runSequence .>> pEnd .>> spaces |>> Ast.Block
let blockSequence = many1 beginEndBlock
let globalParser = blockSequence .>> eof |>> Ast.Ast
```

Note that we have different contexts in which we would like to add error recovery:
* **charSequence embedded in runBlock**: What if we have a comma-separated sequence of characters, some of which do not match a|b|c, but still end with "}"
* **runBlockSequence embedded in beginEndBlock**: What if we have a semicolon-separated sequence of runBlocks, some of which do not match runBlock, but still end with pEnd?
* **beginEndBlock embedded in blockSequence**: What if we have a whitespace-separated sequence of beginEndBlocks, some of which do not match the parser beginEndBlock, but still end with eof?

## General Approach

First of all, we need to extend the discriminated union type `Ast` of our parser by two additional cases:

```
type Ast = 
    ...
    | Escape // used to replace AST subnodes when we recover from an error
    | Error // used to replace the whole AST (at the root level) for severe errors the parser cannot recover from
```

Moreover, we will need a module `ErrRecovery` that contains helper functions to extend our FParsec parser with error recovery. 

First of all, this module will contain a type to add diagnostics during the parsing process:

```
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
        printfn "%s" "^------------------------^\n" 
    member this.DiagnosticsToString = 
        myList
        |> Seq.map string
        |> String.concat "\n"
    member this.Clear = myList.Clear

let ad = Diagnostics() 
```

The types `DiagnosticEmitter`, `DiagnosticSeverity`, `DiagnosticMessage`, and `Diagnostic` we will need for logging any parser error. Since we are dealing with syntax errors,
we will emit diagnostics with the severity 'Error' and the emitter 'Parser', but the classes are more general and could be used also in other contexts, for example to emit also warnings of an interpreter.

The class diagnostics is a mutable list of diagnostics and provides some members to, create, log, print, or get diagnostics.

## Use Cases

Our error recovery should cover the following use cases that we want to complement with (unit) tests:

### Erroneousness charSequence (EcS)
#### EcS1 (complemented by TestEsS01)
```
begin run {a,c,d,a };run{a, b} end
```
In this use case, the first charSequence contains a 'd' but expects 'a'|'b'|'c'.

#### EsS2 (complemented by TestEsS02)
```
begin run {a,c,b, };run{a, b} end
```
In this use case, the first charSequence contains a ',' that is not followed by some 'a'|'b'|'c'.

#### Approach to Error Recovery
To achieve this kind or error recovery, we will modify the original parser 

```
let charSequence = sepBy charChoice comma |>> Ast.Sequence 
```

by the following 

```
let charChoiceBreakCondition = skipUntilLookaheadSeparatorFail comma rightBrace 
let charChoiceErrRec = emitDiagnostics ad charChoiceBreakCondition "charChoice a|b|c expected" 
let charSequence = sepBy (charChoice <|> charChoiceErrRec) comma |>> Ast.Sequence // injection of charChoiceErrRec in choice
```

The function `skipUntilLookaheadSeparatorFail` defined in the module `ErrRecovery`
```
let skipUntilLookaheadSeparatorFail innerSeparator outerSeparator = 
    skipMany (notFollowedBy (attempt innerSeparator <|> outerSeparator) >>. anyChar)
```

is a helper parser that skips any characters until the `innerSeparator` parser succeeds without consuming any input, unless, at the same position, an `outerSeparator` occurs.
In our case, we want to skip the sequence of choices 'a'|'b'|'c' until either the next `comma` parser occurs (without consuming the input `','`), or we finally reach the ending `rightBrace` in the input.

Then, we need an `emitDiagnostics` function from the module `ErrRecovery`.
```
let emitDiagnostics (ad:Diagnostics) escapeParser msg = 
    let errorMsg = DiagnosticMessage msg
    let positionedEscapeParser = 
        getPosition .>>. escapeParser
        |>> fun (pos, escape) -> (pos, escape)
    positionedEscapeParser >>= fun (pos, escape) ->
    let diagnostic = Diagnostic (DiagnosticEmitter.Parser, DiagnosticSeverity.Error,pos,errorMsg)
    ad.AddDiagnostic diagnostic
    preturn () >>% Ast.Escape
```

It is a helper parser applying the `escapeParser` to the input and emitting a diagnostic a the current parsing position with a user-defined error message. The purpose of the message is to explain why this diagnostic was emitted. The returned `Ast.Escape` node is a placeholder for the part of Ast that the parser would otherwise generate. 

In the above use case, the `charChoiceErrRec` parser emits the diagnostic message `"charChoice a|b|c expected"` and applies the `charChoiceBreakCondition` parser to escape from the error situation. The actual escaping, however, happens when we inject `charChoiceErrRec` in

```
let charSequence = sepBy (charChoice <|> charChoiceErrRec) comma |>> Ast.Sequence 
```
This injection enriches the possibilities of our parser to deal with comma-separated lists of cases other than those consumable by `charChoice` alone.

