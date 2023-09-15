module TheParserTypes

type SyntaxNode = 
    | Error
    | String of string
    | Int of int64
    | Escape of unit
    | ManyEitherOr of SyntaxNode list
    | NamedBlock of SyntaxNode
    | NamedBlockList  of SyntaxNode list
