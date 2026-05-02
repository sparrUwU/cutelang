module CuteLang.Tests.TestLexer

open Xunit
open FsUnit.Xunit
open CuteLang.Lexer

[<Fact>]
let ``tokenize function definition`` () =
    let input = "((F_F)>> fact :@ :F x (/>_<)/~o __ x"
    let tokens = tokenize input
    tokens.[0] |> should equal DEFINE
    tokens.[1] |> should equal (ID "fact")
    tokens.[2] |> should equal ARROW
    tokens.[3] |> should equal PARAM
    tokens.[4] |> should equal (ID "x")
    tokens.[5] |> should equal CUTE_O
    tokens.[6] |> should equal UNDERSCORE
    tokens.[7] |> should equal (ID "x")
    tokens.[8] |> should equal EOF

[<Fact>]
let ``tokenize conditional`` () =
    let input = "(?_?) __ B= x 1 (T_T) 1 (E_E) 0"
    let tokens = tokenize input
    tokens.[0] |> should equal COND_IF
    tokens.[1] |> should equal UNDERSCORE
    tokens.[2] |> should equal EQUALS
    tokens.[3] |> should equal (ID "x")
    tokens.[4] |> should equal (INT 1)
    tokens.[5] |> should equal COND_THEN
    tokens.[6] |> should equal (INT 1)
    tokens.[7] |> should equal COND_ELSE
    tokens.[8] |> should equal (INT 0)

[<Fact>]
let ``tokenize cute operators`` () =
    let input = ":+ :- :* :/ :%"
    let tokens = tokenize input
    tokens.[0] |> should equal PLUS
    tokens.[1] |> should equal MINUS
    tokens.[2] |> should equal MULT
    tokens.[3] |> should equal DIV
    tokens.[4] |> should equal MOD

[<Fact>]
let ``tokenize comparisons`` () =
    let input = "B= <B= >B= <B >B !B="
    let tokens = tokenize input
    tokens.[0] |> should equal EQUALS
    tokens.[1] |> should equal LESS_EQ
    tokens.[2] |> should equal GREATER_EQ
    tokens.[3] |> should equal LESS
    tokens.[4] |> should equal GREATER
    tokens.[5] |> should equal NOT_EQ

[<Fact>]
let ``tokenize list operations`` () =
    let input = ">V< head tail []"
    let tokens = tokenize input
    tokens.[0] |> should equal CONS
    tokens.[1] |> should equal HEAD
    tokens.[2] |> should equal TAIL
    tokens.[3] |> should equal NIL

[<Fact>]
let ``tokenize lazy and force`` () =
    let input = "(z_Z) (O_O)!"
    let tokens = tokenize input
    tokens.[0] |> should equal LAZY
    tokens.[1] |> should equal FORCE

[<Fact>]
let ``tokenize io`` () =
    let input = "(^o^)> <(^o^)"
    let tokens = tokenize input
    tokens.[0] |> should equal PRINT
    tokens.[1] |> should equal READ

[<Fact>]
let ``tokenize let binding`` () =
    let input = "(^_^)~ x 42"
    let tokens = tokenize input
    tokens.[0] |> should equal LET
    tokens.[1] |> should equal (ID "x")
    tokens.[2] |> should equal (INT 42)

[<Fact>]
let ``tokenize string literal`` () =
    let input = "\"hello world\""
    let tokens = tokenize input
    tokens.[0] |> should equal (STR "hello world")

[<Fact>]
let ``tokenize full factorial program`` () =
    let input = """((F_F)>> fact :@
    :F x (/>_<)/~o 
        (?_?) __ <B= x 1
        (T_T) 1
        (E_E) __ :* x _ fact _ :- x 1"""
    let tokens = tokenize input
    tokens |> List.length |> should be (greaterThan 10)
    tokens |> List.last |> should equal EOF