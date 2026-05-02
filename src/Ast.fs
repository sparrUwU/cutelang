namespace CuteLang

type Expression =
    | Integer of int
    | String of string
    | Symbol of string
    | Lambda of string * Expression
    | Application of Expression * Expression
    | Conditional of Expression * Expression * Expression
    | LetBinding of string * Expression * Expression
    | BinaryOp of string * Expression * Expression
    | ListLiteral of Expression list
    | EmptyList
    | Head of Expression
    | Tail of Expression
    | Cons of Expression * Expression
    | Lazy of Expression
    | Force of Expression
    | Print of Expression
    | Read

type Definition = {
    Name: string
    Parameters: string list
    Body: Expression
}

type Program = {
    Definitions: Definition list
    MainExpression: Expression option
}