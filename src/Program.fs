open System
open System.IO
open CuteLang.Lexer
open CuteLang.Parser
open CuteLang.Interpreter

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        printfn "Usage: CuteLang <file.ctl>"
        1
    else
        let code = File.ReadAllText args.[0]
        let tokens = Lexer.tokenize code
        Parser.tokens := tokens
        Parser.pos <- 0
        
        try
            let program = Parser.parseProgram ()
            let env = 
                let e = emptyEnv
                let e = { e with Bindings = Map.ofList [ for (k, v) in builtins -> k, v ] }
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
            | Some expr ->
                let result = evaluate expr env
                printfn "=> %A" result
            | None -> ()
            0
        with
        | ex ->
            printfn "Error: %s" ex.Message
            1