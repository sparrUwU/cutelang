namespace CuteLang

open FParsec

module Parser =
    open Ast

    let tokens: Token list ref = ref []
    let mutable pos = 0

    let peek () =
        if pos < (!tokens).Length then Some (!tokens).[pos]
        else None

    let consume () =
        let t = peek ()
        pos <- pos + 1
        t

    let expect (expected: Token) =
        match consume () with
        | Some t when t = expected -> ()
        | Some t -> failwithf "Expected %A, got %A" expected t
        | None -> failwith "Unexpected EOF"

    let parseProgram () =
        let rec parseDefinitions () =
            match peek () with
            | Some DEFINE ->
                expect DEFINE
                match consume () with
                | Some (ID name) ->
                    expect ARROW
                    let rec parseParams () =
                        match peek () with
                        | Some PARAM ->
                            expect PARAM
                            match consume () with
                            | Some (ID param) ->
                                expect CUTE_O
                                param :: parseParams ()
                            | _ -> failwith "Expected parameter name"
                        | _ -> []
                    let parameters = parseParams ()
                    let body = parseExpression ()
                    let def = { Name = name; Parameters = parameters; Body = body }
                    def :: parseDefinitions ()
                | _ -> failwith "Expected function name"
            | Some EOF -> []
            | Some _ ->
                let expr = parseExpression ()
                let def = { Name = "main"; Parameters = []; Body = expr }
                [def]
            | None -> []

        and parseExpression () =
            match peek () with
            | Some (INT n) ->
                expect (INT n)
                Integer n
            | Some (STR s) ->
                expect (STR s)
                String s
            | Some NIL ->
                expect NIL
                EmptyList
            | Some (ID name) ->
                expect (ID name)
                Symbol name
            | Some COND_IF ->
                expect COND_IF
                let cond = parseExpression ()
                expect COND_THEN
                let trueBr = parseExpression ()
                expect COND_ELSE
                let falseBr = parseExpression ()
                Conditional (cond, trueBr, falseBr)
            | Some LET ->
                expect LET
                match consume () with
                | Some (ID name) ->
                    let value = parseExpression ()
                    let body = parseExpression ()
                    LetBinding (name, value, body)
                | _ -> failwith "Expected variable name"
            | Some CONS ->
                expect CONS
                let head = parseExpression ()
                let tail = parseExpression ()
                Cons (head, tail)
            | Some HEAD ->
                expect HEAD
                Head (parseExpression ())
            | Some TAIL ->
                expect TAIL
                Tail (parseExpression ())
            | Some LAZY ->
                expect LAZY
                Lazy (parseExpression ())
            | Some FORCE ->
                expect FORCE
                Force (parseExpression ())
            | Some PRINT ->
                expect PRINT
                Print (parseExpression ())
            | Some READ ->
                expect READ
                Read
            | Some UNDERSCORE ->
                expect UNDERSCORE
                let func = parseExpression ()
                let arg = parseExpression ()
                Application (func, arg)
            | _ ->
                failwithf "Unexpected token in expression: %A" (peek ())

        let defs = parseDefinitions ()
        { Definitions = defs |> List.filter (fun d -> d.Name <> "main") 
          MainExpression = 
            defs 
            |> List.tryFind (fun d -> d.Name = "main") 
            |> Option.map (fun d -> d.Body) }