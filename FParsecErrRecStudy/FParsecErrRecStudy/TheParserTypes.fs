module TheParserTypes

type SyntaxNode = 
    | A 
    | B 
    | C 
    | Sequence of SyntaxNode list
    | Run of SyntaxNode 
    | RunSequence of SyntaxNode list
    | Block of SyntaxNode 
    | Ast of SyntaxNode list
    | Escape
    | Error
