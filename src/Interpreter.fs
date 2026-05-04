namespace CatLang

module Interpreter =

    let rec eval (env: Env) (expr: Expr) : Value =
        match expr with
        | Int n -> VInt n
        | Bool b -> VBool b
        | Unit | EmptyList -> VUnit

        | Var x ->
            match Map.tryFind x env with
            | Some v -> v
            | None -> failwithf "Undefined variable: %s" x

        // let-binding
        | Let (x, e1, e2) ->
            let v1 = eval env e1
            eval (Map.add x v1 env) e2

        // recursive let
        | LetRec (f, e1, e2) ->
            let env' = ref Map.empty
            env' := Map.add f (VRecClosure(f, e1, !env')) env
            eval (!env') e2

        // lambda
        | Lambda (param, body) ->
            VClosure(param, body, env)

        // Function application (самое важное место)
        | App (func, arg) ->
            let fVal = eval env func
            let argVal = eval env arg

            match fVal with
            | VClosure (param, body, clEnv) ->
                eval (Map.add param argVal clEnv) body

            | VRecClosure (f, body, clEnv) ->
                let envWithRec = Map.add f (VRecClosure(f, body, clEnv)) clEnv
                eval (Map.add f (VRecClosure(f, body, envWithRec)) envWithRec) body

            // Встроенные функции
            | _ when func = Var "first_cat" || func = Var "last_cat" ->
                applyBuiltin (if func = Var "first_cat" then "first_cat" else "last_cat") argVal

            | _ -> failwithf "Cannot apply: not a function (got %A)" fVal

        // Binary operations
        | Bin (op, a, b) ->
            let va = match eval env a with VInt n -> n | _ -> failwith "Int expected in binary op"
            let vb = match eval env b with VInt n -> n | _ -> failwith "Int expected in binary op"

            match op with
            | Add -> VInt (va + vb)
            | Sub -> VInt (va - vb)
            | Mul -> VInt (va * vb)
            | Div -> VInt (va / vb)
            | Eq  -> VBool (va = vb)
            | Neq -> VBool (va <> vb)
            | Lt  -> VBool (va < vb)
            | Gt  -> VBool (va > vb)
            | Le  -> VBool (va <= vb)
            | Ge  -> VBool (va >= vb)

        // If expression
        | If (cond, thenExpr, elseExpr) ->
            match eval env cond with
            | VBool true -> eval env thenExpr
            | VBool false -> eval env elseExpr
            | _ -> failwith "If condition must be boolean"

        // List construction
        | Cons (head, tail) ->
            match eval env tail with
            | VList lst -> VList (eval env head :: lst)
            | _ -> failwith "Right side of :: must be a list"

        // IO
        | Print e ->
            let v = eval env e
            printfn "Meow: %A" v
            VUnit

        | Read ->
            try
                System.Console.ReadLine() |> int |> VInt
            with _ -> VInt 0


    // Вспомогательная функция для встроенных list-функций
    and private applyBuiltin (name: string) (arg: Value) : Value =
        match name, arg with
        | "first_cat", VList (h :: _) -> h
        | "last_cat", VList lst when not lst.IsEmpty -> List.last lst
        | "first_cat", VList [] 
        | "last_cat", VList [] -> failwith "first_cat / last_cat: list is empty"
        | _ -> failwithf "Builtin error: %s called with invalid argument" name


    // Главная функция запуска
    let run (code: string) =
        match Parser.parse code with
        | Ok ast ->
            try
                eval Map.empty ast
            with
            | ex -> failwithf "Runtime error: %s" ex.Message
        | Error msg ->
            failwithf "Parse error: %s" msg