module TheParserTypes

type Ast = 
    | A 
    | B 
    | C 
    | Sequence of Ast list
    | Run of Ast 
    | RunSequence of Ast list
    | Block of Ast 
    | Ast of Ast list
    | Escape // used to replace AST subnodes when we recover from an error
    | Error // used to replace the whole AST (at the root level) for severe errors the parser cannot recover from
    | Empty // used to mark empty inner inputs between enclosing ones 
