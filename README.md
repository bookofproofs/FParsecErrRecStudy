# FParsecErrRecStudy

A study of error recovery and how it could be done with FParsec.


## Goal of this study
To add error recovery to a more complex grammar that would recover from errors occurring in many contexts.
In particular, we would like to return at least a partial AST where our parser can succeed in error recovery and emit diagnostics depending on the context in which the error recovery did succeed.
Only a the case none of our context-related error recovery attempts succeed, our partial AST would be replaced by a single ERROR node, while still emitting diagnostics why this happened.

## How to read this README.md

This documentation reflects the repository's formation process, including refactoring, and not its current final version. As a new user of FParsec and a F# beginner, I wanted to share with you the problems I came across when trying to add error recovery to an FParserc parser. 

I think that adding error recovery to a parser that does not support it in an in-built fashion is a non-trivial problem. It is not only a matter of being familiar with functional programming in F# or with FParsec to solve it. So you might be interested with trying out the examples by yourself and see what happens. If not, just stop reading right now and look / use the (still preliminary) final version of the repository's code.

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
"begin run {a,b,a};run{a, b,c} end begin run{a,b,c}; run{a,b} end"
"begin run {a,b,c};run {a} end "
"begin run {a,b,c};run {} end "
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
let globalParser = spaces >>. blockSequence .>> eof |>> Ast.Ast
```

Note that we have different contexts in which we would like to add error recovery:
* **charSequence embedded in runBlock**: What if we have a comma-separated sequence of characters, some of which do not match a|b|c, but still end with "}"
* **runBlockSequence embedded in beginEndBlock**: What if we have a semicolon-separated sequence of runBlocks, some of which do not match runBlock, but still end with pEnd?
* **beginEndBlock embedded in blockSequence**: What if we have a whitespace-separated sequence of beginEndBlocks, some of which do not match the parser beginEndBlock, but still end with eof?

Another consideration is that the grammar deals with whitespaces by consuming any whitespace at the end of each rule production, and, in the globalParser, at the very beginning of the possible input. This is kind of good practice, because it already deals with a lot of special cases we do not want to care about while adding error recovery. However, some grammars (like that of python, for instance) involve significant whitespaces and your grammar definition requires diligent definition in those cases.

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

The class diagnostics is a reference type implemented by System.Collections.Generic.List that stores diagnostics. The type provides some members to create, log, print, or get diagnostics.

## Use Cases

Our error recovery should cover the following use cases that we want to complement with (unit) tests:

### Erroneous charSequence (EcharSequence)
#### EcharSequence01 (complemented by TestEcharSequence01 and TestEcharSequence01Diag)
```
"begin run {a,c,d,a };run{a, b} end"
```
In this use case, the first charSequence contains a 'd' but expects 'a'|'b'|'c'.

#### EcharSequence02 (complemented by TestEcharSequence02 and TestEcharSequence02Diag)
```
"begin run {a,c,b, };run{a, b} end"
```
In this use case, the first charSequence ends by a ',' that is not followed by some 'a'|'b'|'c'.

#### EcharSequence03 (complemented by TestEcharSequence03 and TestEcharSequence03Diag)
```
"begin run {a};run {a, b, ;run{a, b} end"
```
In this use case, the second charSequence ends by ',' that is not followed another charChoice, missing an '}' token.
Escaping the error is still possible, because a third, properly closed charSequence occurs.

#### EcharSequence04 (complemented by TestEcharSequence04 and TestEcharSequence04Diag)
```
"begin run {a,c,a};run { , } ;run{a, b} end"
```
In this use case, the second run block is contains a comma instead of a charSequence.

#### EcharSequence05 (complemented by TestEcharSequence05 and TestEcharSequence05Diag)
```
"begin run {a};run { ;run{a, b} end"
```
In this use case, the second run block does close properly and has no valid charSequence.

#### EcharSequence06 (complemented by TestEcharSequence06 and TestEcharSequence06Diag)
```
"begin run {a};run {{ ;run{a, b} end"
```
In this use case, the second run block does close properly and has two opening curly brackets.

#### Approach to Error Recovery of EcharSequenceXX
To achieve this kind or error recovery, we will modify the original parser 

```
let charSequence = sepBy charChoice comma |>> Ast.Sequence 
```

by the following 

```
let charChoiceBreakCondition = skipUntilLookaheadSeparatorFail comma rightBrace 
let charChoiceErrRec = emitDiagnostics ad charChoiceBreakCondition "charChoice a|b|c expected" 
let charSequence = sepBy (charChoice <|> charChoiceErrRec) comma |>> Ast.Sequence // injection of charChoiceErrRec as an alternative to charChoice
```

The function `skipUntilLookaheadSeparatorFail` defined in the module `ErrRecovery`
```
let skipUntilLookaheadSeparatorFail innerSeparator outerSeparator = 
    skipMany (notFollowedBy (attempt innerSeparator <|> outerSeparator) >>. anyChar)
```

is a helper parser that skips any characters until the `innerSeparator` parser succeeds without consuming any input, unless, at the same position, an `outerSeparator` occurs.
In our case, we want to skip the sequence of choices 'a'|'b'|'c' until either the next `comma` parser occurs (without consuming the input `','`), or we finally reach the ending `rightBrace` in the input. Of course, it only covers some  

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
This injection enriches the possibilities of our parser to deal with erroneous charChoices.

### Erroneous runSequence (ErunSequence)
#### Approach by analogy to EcharSequence
The recovery mechanism described in the previous EcharSequencex section is applicable for other parsers in our grammar because it is independent from the specific parser. It should work whenever we have a sequence of something separated by a separator that we can parse via the parser 

```
let parser = sepBy something separator
```

In our grammar, this pattern applies both for 

```
let charSequence = sepBy charChoice comma |>> Ast.Sequence 
```

and for 

```
let runSequence = sepBy runBlock semicolon .>> spaces |>> Ast.RunSequence
```

Therefore, we can reproduce the mechanism also there by the following 

```
// original parser
// let runSequence = sepBy runBlock semicolon |>> Ast.RunSequence 
// modifications adding error recovery to runSequence:
let runBlockBreakCondition = skipUntilLookaheadSeparatorFail semicolon pEnd
let runBlockErrRec = emitDiagnostics ad runBlockBreakCondition "run block expected"
let runSequence = sepBy (runBlock <|> runBlockErrRec) semicolon .>> spaces |>> Ast.RunSequence // injection of runBlockErrRec as an alternative to runBlock 
```

Now, we can complement our use cases EcharSequence01, 02, ... by the corresponding use cases ErunSequence01, 02, ....

#### ErunSequence01 (complemented by TestErunSequence01 and TestErunSequence01Diag)
```
"begin run {a};xxx {a, b};run{a, b} end begin run{a,b,c}; run{a,b} end "
```
In this use case, the second runBlock is not started properly.

#### ErunSequence02 (complemented by TestErunSequence02 and TestErunSequence02Diag)
```
"begin run {a};run {a, b}; end begin run{a,b,c}; run{a,b} end"
```
In this use case, there is a third runBlock missing before the 'end' token.

#### ErunSequence03 (complemented by TestErunSequence03 and TestErunSequence03Diag)
```
"begin run {a};run {a, b}; begin run{a,b,c}; run{a,b} end"
```
In this use case, the second runSequence ends by ';' that is not followed another runBlock, missing an 'end' token.
Escaping the error is still possible, because a third, properly closed runSequence occurs.

#### ErunSequence04 (complemented by TestErunSequence04 and TestErunSequence04Diag)
```
"begin run {a} end begin ; end"
```
In this use case, the second begin end block is contains a semicolon instead of a runSequence.

#### ErunSequence05 (complemented by TestErunSequence05 and TestErunSequence05Diag)
```
"begin run {a} end begin begin run{b} end"
```
In this use case, the second begin end block does close properly and has no valid runSequence.

#### ErunSequence06 (complemented by TestErunSequence06 and TestErunSequence06Diag)
```
"begin run {a} end begin begin end begin run{b} end"
```
In this use case, the second begin end block does close properly but has two opening 'begin' tokens.

### Erroneous runBlock (ErunBlock)
So far, we can handle the most common errors that can occur in a sequence separated by some separator. 

A common situation in grammars (e.g. of many programming languages) is that such sequences are enclosed by some opening and some closing tokens. For instance, 
many programming languages support a list of comma-separated arguments enclosed by opening and closing parentheses.

In our grammar, there are two enclosing types of tokens, `leftBrace` and `rightBrace` in `runBlock` as well as `pBegin` and `pEnd` in `beginEndBlock`.

```
let runBlock = (pRun >>. leftBrace >>. charSequence) .>> rightBrace |>> Ast.Run
...
let beginEndBlock = pBegin >>. runSequence .>> pEnd .>> spaces |>> Ast.Block
...
```

The common pattern here is that we have some opening input `a`, followed by some inner input `b`, followed by some closing input `c` that we want to match. 
Note that our error recovery and use cases so far only care about errors that can occur in `b`, being a sequence of something. While our grammar supports `a b c` (with no error), we still do not cover errors of the following types:
1. `a b` (closing `c` is missing),
1. `b c` (opening `a` is missing),
1. `a c` (`b` is empty),
1. `b` (opening `a` and closing `c` are missing),
1. `c` (`a` and `b` are missing),
1. `a` (`b` and `c` are missing).

The attentive reader will notice that the case "`b` is missing" is already covered by our grammar. From the very beginning, `charSequence` and `runSequence` were allowed to be empty, because `sepBy` requires 0 or more occurrences. 

```
let charSequence = sepBy charChoice comma |>> Ast.Sequence 
...
let runSequence = sepBy runBlock semicolon |>> Ast.RunSequence 
...
```

Unfortunately, we have modified their original definitions into 

```
let charSequence = sepBy (charChoice <|> charChoiceErrRec) comma |>> Ast.Sequence 
...
let runSequence =  sepBy (runBlock <|> runBlockErrRec) semicolon |>> Ast.RunSequence 
...
```
With these modifications, given empty sequences of `charChoice` or `runBlock`, the parser will also try out our error recovery versions of them `charChoiceErrRec`, respectively `runBlockErrRec`. This has the side effect that they will emit the diagnostics `"charChoice a|b|c expected"`, respectively `"run block expected"`, if the sequence was empty.

We have to find a way to deal with these false positives of our error recovery.

After many fruitless attempts to somehow fix the existing recovery mechanism, the basic idea to make progress was to invent a new helper parser `abc` that would check the new cases and emit corresponding diagnostics, if something was missing. It turns out that such a parser is possible. The most tricky part of writing that parser was to find out a balanced way of in which order the parser has to try out the different cases while parsing the input before it definitely knows that it has to emit some diagnostics.

This is the parser (which we add to the `ErrRecovery` module.)

```
let abc a b c (aName:string) (bName:string) (cName:string) (ad:Diagnostics) =
    let aMissing = 
        getPosition >>= fun pos -> 
        b .>> c >>= fun r -> 
            emitDiagnostics1 ad ("missing opening " + aName) pos |> ignore
            preturn r
    let cMissing = 
        getPosition >>= fun pos -> 
        a >>. b >>= fun r -> 
            emitDiagnostics1 ad ("missing closing " + cName) pos |> ignore
            preturn r
    let acMissing = 
        getPosition >>= fun pos -> 
        b >>= fun r -> 
            emitDiagnostics1 ad ("missing opening " + aName) pos |> ignore
            getPosition >>= fun pos -> 
            emitDiagnostics1 ad ("missing closing " + cName) pos |> ignore
            preturn r
    let bMissing = a >>. c >>% Ast.Empty
   
    attempt bMissing <|> 
    attempt (a >>. b .>> c) <|> cMissing
    <|> (attempt aMissing <|> acMissing)
```


Our version of the `abc` parser reflects the fact that we only want to match the enclosing `a` and `c` but do not want to add them to the abstract syntax tree. Moreover, it reflects the fact that `bMissing` is syntactically correct. There might be other versions of `abc` parsers needed for other grammars (we do not cover them here.) 

Note that our version of `abc` first attempts to parse the input with the case `bMissing`, followed by another attempt to parse a `b` which is syntactically correctly enclosed by `a` and `c`. Those two cases emit no diagnostics, all other cases do.

We also had to introduce a new Ast type
```
type Ast = 
    ...
    | Empty // used to mark empty inner inputs between enclosing ones 

```
because otherwise, the F# type inference would have problems with matching bMissing (being a unit) with a complete enclosed list (being Ast).

To use the `abc` parser for error recovery, we have to modify our grammar once again. All we need to do is to replace `runBlock` by a `runBlockAbc` version:

```
// original parser
// let runSequence = sepBy runBlock semicolon |>> Ast.RunSequence 

// modifications after first adding error of recovery to runSequence:
// let runBlockBreakCondition = skipUntilLookaheadSeparatorFail semicolon pEnd
// let runBlockErrRec = emitDiagnostics ad runBlockBreakCondition "run block expected"
// let runBlock = (pRun >>. leftBrace >>. charSequence) .>> rightBrace |>> Ast.Run
// let runSequence = sepBy (runBlock <|> runBlockErrRec) semicolon .>> spaces |>> Ast.RunSequence // injection of runBlockErrRec as an alternative to runBlock 

// modifications adding error recovery to runBlock:
let runBlockAbc = pRun >>. abc leftBrace charSequence rightBrace "{" "charSequence" "}" ad |>> Ast.Run
let runBlockBreakCondition = skipUntilLookaheadSeparatorFail semicolon pEnd
let runBlockErrRec = emitDiagnostics ad runBlockBreakCondition "run block expected"
let runSequence = sepBy (runBlockAbc <|> runBlockErrRec) semicolon .>> spaces |>> Ast.RunSequence // replacement of (runBlock <|> runBlockErrRec) by (runBlockABC <|> runBlockErrRec) 
```

#### ErunBlock01 (complemented by TestErunBlock01 and TestErunBlock01Diag)
```
"begin run {a};run a, b };run{a, b} end"
```
In this use case, the second run block is missing an opening "{".
#### ErunBlock02 (complemented by TestErunBlock02 and TestErunBlock02Diag)
```
"begin run {a,c,a};run a,c ;run{a, b} end"
```
In this use case, the second run block is missing both, an opening "{", and a closing "}".
#### ErunBlock03 (complemented by TestErunBlock03 and TestErunBlock03Diag)
```
"begin run {a,c,a};run { } ;run{a, b} end"
```
In this use case, the second run block is empty.

#### ErunBlock04 (complemented by TestErunBlock04 and TestErunBlock04Diag)
```
"begin run {a};run { a, b ;run{a, b} end"
```
In this use case, the second run block is missing a closing "}".

#### ErunBlock05 (complemented by TestErunBlock05 and TestErunBlock05Diag)
```
"begin run {a};run };run{a, b} end"
```
In this use case, the second run block is missing an opening "{" and a charSequence.

#### ErunBlock06 (complemented by TestErunBlock06 and TestErunBlock06Diag)
```
"begin run {a};run { ;run{b} end"
```
In this use case, the second run block contains only an opening "{".

### Erroneous beginEndBlock (EbeginEndBlock)

By analogy, we could do the same modifications to `beginEndBlock`, replacing it by a version of `beginEndBlockAbc`. 

```
// original parser
// let beginEndBlock = pBegin >>. runSequence .>> pEnd .>> spaces |>> Ast.Block
// let blockSequence = (many1 beginEndBlock) .>> spaces

// modifications adding error recovery to beginEndBlock:
let beginEndBlockAbc = abc pBegin runSequence pEnd "begin" "runSequence" "end" ad |>> Ast.Block
let blockSequence = (many1 beginEndBlockAbc) .>> spaces // replacement by the 'abc' version of beginEndBlock
```
However, it turns out that all unit tests we have created and tested successful so far now fail with the exception

```
The combinator 'many' was applied to a parser that succeeds without consuming input and without changing the parser state in any other way. (If no exception had been raised, the combinator likely would have entered an infinite loop.) 
```

Obviously we have a bug in our grammar because our new `beginEndBlockAbc` can now succeed without consuming any input. How can this possibly be? 

A closer look at our `abc` parser reveals that this actually can happen if some of the choices tried out in `abc` succeed without consuming any input.

Debugging a parser can be very challenging, and there are ways to do it (e.g. following the [approach from the FParsec Documentation](https://www.quanttec.com/fparsec/users-guide/debugging-a-parser.html). However, we have already some error recovery so we can make the parser output it after it threw the exception:

```
try
    ad.Clear()
    let input = "begin run {a} end"
    let result = run (many1 beginEndBlockAbc) input
    printf "%O\n" result
    ad.PrintDiagnostics
with
| :? System.Exception as ex -> 
    ad.PrintDiagnostics
    printfn "An error occurred: %s" ex.Message
```

We get the following diagnostics:

```
Diagnostic
  (Parser, Error, (Ln: 1, Col: 18), DiagnosticMessage "run block expected")
Diagnostic
  (Parser, Error, (Ln: 1, Col: 18), DiagnosticMessage "run block expected")
Diagnostic
  (Parser, Error, (Ln: 1, Col: 18), DiagnosticMessage "missing opening begin")
Diagnostic
  (Parser, Error, (Ln: 1, Col: 18), DiagnosticMessage "missing closing end")
^------------------------^

An error occurred: (Ln: 1, Col: 35): The combinator `many` was applied to a parser that succeeds without consuming input and without changing the parser state in any other way. (If no exception had been raised, the combinator likely would have entered an infinite loop.)
```

Obviously, our parser expects yet another `beginEndBlock` at the end of file. This is a hint that our grammar is not correct. A closer look at the `pEnd` parser reveals that we are consuming any optional spaces, although we are expecting significant spaces that would separate parts of the input stream that can be consumed by our `beginEndBlock` parser. So what we actually want to do is not to consume any trailing insignificant spaces after `pEnd` but use them as significant separators. We have to make the following corrections:

```
// let pEnd = skipString "end" >>. spaces 
// becomes
let pEnd = skipString "end" 
...

// let blockSequence = many1 beginEndBlockAbc
// becomes
let blockSequence = sepEndBy1 beginEndBlockAbc spaces1 
```

Our grammar is now correct. But some of our unit tests that worked before the change still fail. The reason are trailing spaces in the input strings of these failing unit tests. This is weird, because the FParsec Documentation tells us

> `sepEndBy1 p sep` parses *one* or more occurrences of `p` separated and optionally ended by `sep` (in EBNF notation: `p (sep p)* sep?`). It returns a list of the results returned by `p`.

Thus, `sepEndBy1 beginEndBlockAbc spaces1`  should ignore the last occurrence of trailing `spaces1`. Why doesn't it? 

Again, our diagnostics help us in this case. For instance, if the trailing spaces occur at the 67th column of the input string, the following additional diagnostics regarding the trailing spaces are emitted:
```
Diagnostic
  (Parser, Error, (Ln: 1, Col: 67), DiagnosticMessage "run block expected")
Diagnostic
  (Parser, Error, (Ln: 1, Col: 67), DiagnosticMessage "run block expected")
Diagnostic
  (Parser, Error, (Ln: 1, Col: 67), DiagnosticMessage "missing opening begin")
Diagnostic
  (Parser, Error, (Ln: 1, Col: 67), DiagnosticMessage "missing closing end")
```
Because these diagnostics are emitted, our parser does not fail. Consequently, `sepEndBy1` fails because it awaits yet another occurrence of `beginEndBlockAbc`.
Only if our parser failed at trailing spaces, the `sepEndBy1` would succeed because it would correctly ignore trailing spaces. 

Adding this special case in our `abc` helper parser would be difficult because we need to use it in different contexts and not only at the top level of our parser before the input stream ends. 

We make the decide to "solve" our false positive diagnostics by removing the trailing spaces from the inputs to our failing unit tests. This is a trade-off between simplicity and functionality. Our error recovery is not perfect but it covers 'most common' syntax errors that can occur in our grammar. This way, we treat trailing spaces as another syntax error the user has to correct.

What we gain by this decision is that we also can add new syntax errors our error recovery correctly supports, because we can complement the use cases ErunBlock01, 02, ... by corresponding use cases EbeginEndBlock01, 02, ...

#### EbeginEndBlock01 (complemented by TestEbeginEndBlock01 and TestEbeginEndBlock01Diag)
```
"begin run {a} end run {b} end begin run{c} end"
```
In this use case, the second beginEnd block is missing an opening "begin".
#### EbeginEndBlock02 (complemented by TestEbeginEndBlock02 and TestEbeginEndBlock02Diag)
```
"begin run {a} end run {b} begin run{c} end"
```
In this use case, the second beginEnd block is missing both, an opening "begin", and a closing "end".
#### EbeginEndBlock03 (complemented by TestEbeginEndBlock03 and TestEbeginEndBlock03Diag)
```
"begin run {a} end begin end begin run{c} end"
```
In this use case, the second beginEnd block is empty (that is syntactically correct in our grammar)

#### EbeginEndBlock04 (complemented by TestEbeginEndBlock04 and TestEbeginEndBlock04Diag)
```
"begin run {a} end begin run {b} begin run{c} end"
```
In this use case, the second beginEnd block is missing a closing "end".

#### EbeginEndBlock05 (complemented by TestEbeginEndBlock05 and TestEbeginEndBlock05Diag)
```
"begin run {a} end end begin run{c} end"
```
In this use case, the second beginEnd block is missing an opening "begin" and a runSequence.

#### EbeginEndBlock06 (complemented by TestEbeginEndBlock06 and TestEbeginEndBlock06Diag)
```
"begin run {a} end begin begin run{c} end"
```
In this use case, the second beginEnd block only contains an opening "begin".

#### EbeginEndBlock07 (complemented by TestEbeginEndBlock07 and TestEbeginEndBlock07Diag)
```
"begin run {a};run{b} end xxxbegin run{b,c} end begin run{c}; run{c} end"
```
In this use case, the second block does not start properly.

### Bottom Line: Adding error recovery to a parser is hard!

The FParsec parser generator does not support in-built error recovery. However, it is possible to add one on your own. 

Takeaways from the study:

* No **"one-fits-all"**: You have to add new parser combinators to the repertoire of FParserc that highly depend on the requirements of your particular grammar.
* **To fail or not to fail?**: Many FParsec parser combinators depend on whether input parsers fail or not. The most challenging part is to find a balance between allowing your parsers to fail or not to fail and emit diagnostics instead. The latter will affect the way FParsec standard parser combinators work. 
* Your original grammar without error recovery will in a subtle way differ from the grammar after you add error recovery to it. 
* Adding error recovery and debugging it when it doesn't work as expected is hard.
