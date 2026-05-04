open CatLang
open System.IO

[<EntryPoint>]
let main argv =
    let file = if argv.Length > 0 then argv[0] else "program.cat"
    printfn "=== CatLang Interpreter ===\n"
    
    try
        let code = File.ReadAllText(file)
        printfn "%s\n" code
        let result = Interpreter.run code
        printfn "\nFinal result: %A" result
    with
    | ex -> printfn "Error: %s" ex.Message
    
    0