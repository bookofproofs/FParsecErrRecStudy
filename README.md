# FParsecErrRecStudy

A study of error recovery and how it could be done with FParsec.


## Goal of this study
To add error recovery to a more complex grammar that would recover from errors occuring in many contexts.

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
type SyntaxNode = 
    | A 
    | B 
    | C 
    | Sequence of SyntaxNode list
    | Run of SyntaxNode 
    | RunSequence of SyntaxNode list
    | Block of SyntaxNode 
    | Ast of SyntaxNode list
    
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
let charSequence = sepBy charChoice comma |>> SyntaxNode.Sequence 
let runBlock = (pRun >>. leftBrace >>. spaces >>. charSequence .>> spaces) .>> rightBrace .>> spaces |>> SyntaxNode.Run
let runSequence = sepBy runBlock semicolon |>> SyntaxNode.RunSequence 
let beginEndBlock = pBegin >>. runSequence .>> pEnd .>> spaces |>> SyntaxNode.Block
let blockSequence = many1 beginEndBlock
let globalParser = blockSequence .>> eof |>> SyntaxNode.Ast
```

Note that we have different contexts in which we would like to add error recovery:
* **charSequence embedded in runBlock**: What if we have a comma-separated sequence of characters, some of which do not match a|b|c, but still end with "}"
* **runBlockSequence embedded in beginEndBlock**: What if we have a semicolon-separated sequence of runBlocks, some of which do not match runBlock, but still end with pEnd?
** **beginEndBlock embedded in blockSequence**: What if we have a whitespace-separated sequence of beginEndBlocks, some of which do not match the parser beginEndBlock, but still end with eof?

