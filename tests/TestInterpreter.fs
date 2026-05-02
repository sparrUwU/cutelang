module CuteLang.Tests.TestInterpreter

open Xunit
open FsUnit.Xunit
open CuteLang.Lexer
open CuteLang.Parser
open CuteLang.Interpreter
open CuteLang.Ast

let evalString (input: string) =
    let tokens = Lexer.tokenize input
    Parser.tokens := tokens
    Parser.pos <- 0
    let program = Parser.parseProgram ()
    let env = 
        let e = emptyEnv
        let e = { e with Bindings = builtins }
        List.fold (fun (e: Env) (def: Definition) ->
            let rec curry params body =
                match params with
                | [] -> body
                | p::ps -> Lambda (p, curry ps body)
            let expr = curry def.Parameters def.Body
            let value = evaluate expr e
            extendEnv def.Name value e
        ) e program.Definitions
    match program.MainExpression with
    | Some expr -> Some (evaluate expr env)
    | None -> None

[<Fact>]
let ``evaluate integer literal`` () =
    let result = evalString "42"
    result |> should equal (Some (VInt 42))

[<Fact>]
let ``evaluate simple arithmetic`` () =
    let result = evalString "__ :+ 2 3"
    result |> should equal (Some (VInt 5))

[<Fact>]
let ``evaluate conditional true`` () =
    let result = evalString "(?_?) __ B= 1 1 (T_T) 42 (E_E) 0"
    result |> should equal (Some (VInt 42))

[<Fact>]
let ``evaluate conditional false`` () =
    let result = evalString "(?_?) __ B= 1 2 (T_T) 42 (E_E) 0"
    result |> should equal (Some (VInt 0))

[<Fact>]
let ``evaluate factorial of 5`` () =
    let code = """((F_F)>> sub1 :@
    :F x (/>_<)/~o __ :- x 1

((F_F)>> fact :@
    :F x (/>_<)/~o 
        (?_?) __ <B= x 1
        (T_T) 1
        (E_E) __ :* x _ fact _ sub1 x

__ fact 5"""
    let result = evalString code
    result |> should equal (Some (VInt 120))

[<Fact>]
let ``evaluate list head`` () =
    let code = "__ head __ >V< 1 >V< 2 >V< 3 []"
    let result = evalString code
    result |> should equal (Some (VInt 1))

[<Fact>]
let ``evaluate list tail`` () =
    let code = "__ head __ tail __ >V< 1 >V< 2 >V< 3 []"
    let result = evalString code
    result |> should equal (Some (VInt 2))

[<Fact>]
let ``evaluate let binding`` () =
    let code = "(^_^)~ x 42 __ :+ x x"
    let result = evalString code
    result |> should equal (Some (VInt 84))

[<Fact>]
let ``evaluate fibonacci`` () =
    let code = """((F_F)>> fib :@
    :F n (/>_<)/~o 
        (?_?) __ <B= n 1
        (T_T) n
        (E_E) __ :+ _ fib __ :- n 1 _ fib __ :- n 2

__ fib 7"""
    let result = evalString code
    result |> should equal (Some (VInt 13))

[<Fact>]
let ``greater than comparison works`` () =
    let result = evalString "(?_?) __ >B 5 3 (T_T) 100 (E_E) 0"
    result |> should equal (Some (VInt 100))

[<Fact>]
let ``not equals comparison works`` () =
    let result = evalString "(?_?) __ !B= 1 2 (T_T) 1 (E_E) 0"
    result |> should equal (Some (VInt 1))