module CuteLang.Tests.TestParser

open Xunit
open FsUnit.Xunit
open CuteLang.Lexer
open CuteLang.Parser
open CuteLang.Ast

let parseString (input: string) =
    let tokens = Lexer.tokenize input
    Parser.tokens := tokens
    Parser.pos <- 0
    Parser.parseProgram ()

[<Fact>]
let ``parse simple function`` () =
    let program = parseString "((F_F)>> id :@ :F x (/>_<)/~o __ x"
    program.Definitions.Length |> should equal 1
    let def = program.Definitions.[0]
    def.Name |> should equal "id"
    def.Parameters |> should equal ["x"]

[<Fact>]
let ``parse multi-param function`` () =
    let program = parseString "((F_F)>> add :@ :F x (/>_<)/~o :F y (/>_<)/~o __ :+ x y"
    program.Definitions.Length |> should equal 1
    let def = program.Definitions.[0]
    def.Name |> should equal "add"
    def.Parameters.Length |> should equal 2
    def.Parameters.[0] |> should equal "x"
    def.Parameters.[1] |> should equal "y"

[<Fact>]
let ``parse conditional`` () =
    let program = parseString "((F_F)>> test :@ :F x (/>_<)/~o (?_?) __ B= x 1 (T_T) 1 (E_E) 0"
    program.Definitions.Length |> should equal 1
    
[<Fact>]
let ``parse lambda application`` () =
    let input = "((F_F)>> apply :@ :F f (/>_<)/~o :F x (/>_<)/~o __ f x"
    let program = parseString input
    program.Definitions.Length |> should equal 1
    
[<Fact>]
let ``parse list construction`` () =
    let input = "((F_F)>> makeList :@ __ >V< 1 >V< 2 []"
    let program = parseString input
    program.Definitions.Length |> should equal 1