namespace CatLang

type BinOp =
    | Add               // :)
    | Sub               // :(
    | Mul               // :*
    | Div               // }:(
    | Eq                // same_cats
    | Neq               // not_same_cats
    | Lt                // smaller_cat
    | Gt                // bigger_cat
    | Le                // smol_or_equal_cat
    | Ge                // big_or_equal_cat

type Pattern =
    | PWildcard
    | PVar of string
    | PInt of int
    | PBool of bool
    | PCons of Pattern * Pattern
    | PEmptyList

type Expr =
    | Int of int
    | Bool of bool
    | Unit
    | Var of string
    | Let of string * Expr * Expr           // meow x 0.0 ...
    | LetRec of string * Expr * Expr        // play_again!
    | Lambda of string * Expr               // cat_game
    | App of Expr * Expr                    // please
    | Bin of BinOp * Expr * Expr
    | If of Expr * Expr * Expr              // UwU ... happy:) ... OwO
    | Match of Expr * (Pattern * Expr) list // two_halves:3 ... love>.<
    | EmptyList                             // hug()
    | Cons of Expr * Expr                   // ::
    | Print of Expr                         // say:3
    | Read                                  // listen:3