namespace CatLang

type Value =
    | VInt of int
    | VBool of bool
    | VUnit
    | VClosure of string * Expr * Env      // обычное замыкание
    | VRecClosure of string * Expr * Env   // для рекурсии
    | VList of Value list

and Env = Map<string, Value>