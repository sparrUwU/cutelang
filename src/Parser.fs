namespace CatLang
open FParsec

#nowarn "40"

type Parser<'t> = Parser<'t, unit>

module Parser =

    let ws = spaces
    let str s = pstring s .>> ws

    let pInt = pint32 .>> ws |>> Int
    let pTrue = str "yesss;3" >>% Bool true
    let pFalse = str "nooo:(" >>% Bool false
    let pUnit = str "hug()" >>% EmptyList

    let pVar = identifier (IdentifierOptions()) .>> ws |>> Var

    let rec term = 
        choice [
            pLambda
            pLet
            pLetRec
            pIf
            pApp
            pCons
            pAtom
            between (str "(") (str ")") term
        ] .>> ws

    and pAtom = 
        choice [pInt; pTrue; pFalse; pUnit; pVar]

    and pApp = 
        pAtom .>> str "please" .>>. term |>> App

    and pCons = 
        pAtom .>> str "::" .>>. term |>> Cons

    and pLambda =
        str "cat_game" >>. many1 (identifier(IdentifierOptions()) .>> ws) .>> str "->" .>>. term
        |>> fun (args, body) -> List.foldBack (fun x e -> Lambda(x, e)) args body

    and pLet =
        str "meow" >>. pVar .>> str "0.0" .>>. term .>> str "nya" .>>. term
        |>> fun ((Var name, e1), e2) -> Let(name, e1, e2)

    and pLetRec =
        str "play_again!" >>. pVar .>> str "0.0" .>>. term .>> str "nya" .>>. term
        |>> fun ((Var name, e1), e2) -> LetRec(name, e1, e2)

    and pIf =
        str "UwU" >>. term .>> str "happy:)" .>>. term .>> str "OwO" .>>. term
        |>> fun ((cond, thenExpr), elseExpr) -> If(cond, thenExpr, elseExpr)

    // IO
    let pPrint = str "say:3" >>. term |>> Print
    let pRead : Parser<Expr> = str "listen:3" >>% Read

    let parse (input: string) =
        match run (term .>> eof) input with
        | Success (result, _, _) -> Result.Ok result
        | Failure (msg, _, _) -> Result.Error msg