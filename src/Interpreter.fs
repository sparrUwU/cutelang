namespace CuteLang

module Interpreter =
    open System
    open Ast

    type Value =
        | VInt of int
        | VString of string
        | VBool of bool
        | VList of Value list
        | VClosure of string * Expression * Env
        | VBuiltin of (Value -> Value)
        | VThunk of Expression * Env ref
        | VUnit

    and Env = {
        Bindings: Map<string, Value>
        Parent: Env option
    }

    let emptyEnv = { Bindings = Map.empty; Parent = None }

    let extendEnv name value env =
        { Bindings = Map.add name value env.Bindings; Parent = Some env }

    let lookup name (env: Env) =
        match Map.tryFind name env.Bindings with
        | Some v -> v
        | None ->
            match env.Parent with
            | Some p -> lookup name p
            | None -> failwithf "Undefined: %s" name

    let builtins =
        Map.ofList [
            "head", VBuiltin (function VList (h::_) -> h | _ -> failwith "head: empty list")
            "tail", VBuiltin (function VList (_::t) -> VList t | _ -> failwith "tail: empty list")
            "cons", VBuiltin (function 
                | VClosure _ -> failwith "cons: not fully applied"
                | v -> VBuiltin (function l -> VList (v :: (match l with VList lst -> lst | _ -> [v]))))
        ]

    let rec evaluate (expr: Expression) (env: Env) : Value =
        match expr with
        | Integer n -> VInt n
        | String s -> VString s
        | Symbol name -> lookup name env
        | EmptyList -> VList []
        | ListLiteral exprs -> VList (List.map (fun e -> evaluate e env) exprs)
        
        | Lambda (param, body) -> VClosure (param, body, env)
        
        | Application (func, arg) ->
            let f = evaluate func env
            let a = evaluate arg env
            apply f a
        
        | Conditional (cond, t, f) ->
            let c = evaluate cond env
            match c with
            | VBool true -> evaluate t env
            | VBool false -> evaluate f env
            | _ -> failwith "Condition must be boolean"
        
        | LetBinding (name, value, body) ->
            let v = evaluate value env
            let newEnv = extendEnv name v env
            evaluate body newEnv
        
        | BinaryOp (op, left, right) ->
            let l = evaluate left env
            let r = evaluate right env
            match op, l, r with
            | ":+", VInt a, VInt b -> VInt (a + b)
            | ":-", VInt a, VInt b -> VInt (a - b)
            | ":*", VInt a, VInt b -> VInt (a * b)
            | ":/", VInt a, VInt b -> VInt (a / b)
            | ":%", VInt a, VInt b -> VInt (a % b)
            | "B=", a, b -> VBool (a = b)
            | "<B=", VInt a, VInt b -> VBool (a <= b)
            | ">B=", VInt a, VInt b -> VBool (a >= b)
            | "<B", VInt a, VInt b -> VBool (a < b)
            | ">B", VInt a, VInt b -> VBool (a > b)
            | "!B=", a, b -> VBool (a <> b)
            | _ -> failwithf "Unknown operator: %s" op
        
        | Head e ->
            match evaluate e env with
            | VList (h::_) -> h
            | _ -> failwith "head: not a list"
        
        | Tail e ->
            match evaluate e env with
            | VList (_::t) -> VList t
            | _ -> failwith "tail: not a list"
        
        | Cons (h, t) ->
            let head = evaluate h env
            let tail = evaluate t env
            match tail with
            | VList lst -> VList (head :: lst)
            | _ -> failwith "cons: tail must be list"
        
        | Lazy e -> VThunk (e, ref env)
        | Force e ->
            match evaluate e env with
            | VThunk (expr, thunkEnv) -> evaluate expr !thunkEnv
            | v -> v
        
        | Print e ->
            let v = evaluate e env
            printfn "%A" v
            VUnit
        
        | Read ->
            let line = Console.ReadLine()
            VString line

    and apply (func: Value) (arg: Value) : Value =
        match func with
        | VClosure (param, body, env) ->
            let newEnv = extendEnv param arg env
            evaluate body newEnv
        | VBuiltin f -> f arg
        | _ -> failwithf "Not a function: %A" func