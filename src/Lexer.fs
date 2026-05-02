namespace CuteLang

module Lexer =
    type Token =
        | DEFINE
        | ARROW
        | PARAM
        | LET
        | COND_IF
        | COND_THEN
        | COND_ELSE
        | EQUALS
        | LESS_EQ
        | GREATER_EQ
        | LESS
        | GREATER
        | NOT_EQ
        | CONS
        | HEAD
        | TAIL
        | NIL
        | PLUS
        | MINUS
        | MULT
        | DIV
        | MOD
        | LAZY
        | FORCE
        | PRINT
        | READ
        | LPAREN
        | RPAREN
        | UNDERSCORE
        | CUTE_O
        | INT of int
        | STR of string
        | ID of string
        | EOF

    let tokenize (input: string) : Token list =
        let mutable tokens = []
        let mutable i = 0
        
        let rec skipWhitespace () =
            while i < input.Length && (input.[i] = ' ' || input.[i] = '\t' || input.[i] = '\n' || input.[i] = '\r') do
                i <- i + 1

        let rec readWhile (pred: char -> bool) =
            let start = i
            while i < input.Length && pred input.[i] do
                i <- i + 1
            input.[start..i-1]

        while i < input.Length do
            skipWhitespace ()
            if i >= input.Length then ()
            else
                let c = input.[i]
                match c with
                | '(' when i + 8 <= input.Length && input.[i..i+7] = "((F_F)>>" ->
                    tokens <- DEFINE :: tokens; i <- i + 8
                | '(' when i + 4 <= input.Length && input.[i..i+3] = "(^_^)" ->
                    tokens <- LET :: tokens; i <- i + 5
                | '(' when i + 4 <= input.Length && input.[i..i+3] = "(?_?)" ->
                    tokens <- COND_IF :: tokens; i <- i + 4
                | '(' when i + 4 <= input.Length && input.[i..i+3] = "(T_T)" ->
                    tokens <- COND_THEN :: tokens; i <- i + 4
                | '(' when i + 4 <= input.Length && input.[i..i+3] = "(E_E)" ->
                    tokens <- COND_ELSE :: tokens; i <- i + 4
                | '(' when i + 4 <= input.Length && input.[i..i+3] = "(z_Z)" ->
                    tokens <- LAZY :: tokens; i <- i + 4
                | '(' when i + 5 <= input.Length && input.[i..i+4] = "(O_O)!" ->
                    tokens <- FORCE :: tokens; i <- i + 5
                | '(' when i + 4 <= input.Length && input.[i..i+3] = "(^o^)" ->
                    tokens <- PRINT :: tokens; i <- i + 5
                | '<' when i + 4 <= input.Length && input.[i..i+4] = "<(^o^)" ->
                    tokens <- READ :: tokens; i <- i + 5
                | '(' when i + 7 <= input.Length && input.[i..i+7] = "(/>_<)/~o" ->
                    tokens <- CUTE_O :: tokens; i <- i + 8
                | ':' when i + 1 < input.Length ->
                    i <- i + 1
                    match input.[i] with
                    | '@' -> tokens <- ARROW :: tokens; i <- i + 1
                    | 'F' -> tokens <- PARAM :: tokens; i <- i + 1
                    | '+' -> tokens <- PLUS :: tokens; i <- i + 1
                    | '-' -> tokens <- MINUS :: tokens; i <- i + 1
                    | '*' -> tokens <- MULT :: tokens; i <- i + 1
                    | '/' -> tokens <- DIV :: tokens; i <- i + 1
                    | '%' -> tokens <- MOD :: tokens; i <- i + 1
                    | _ -> failwithf "Unknown token: :%c" input.[i]
                | '>' when i + 2 <= input.Length && input.[i..i+2] = ">B=" ->
                    tokens <- GREATER_EQ :: tokens; i <- i + 3
                | '<' when i + 2 <= input.Length && input.[i..i+2] = "<B=" ->
                    tokens <- LESS_EQ :: tokens; i <- i + 3
                | '!' when i + 2 <= input.Length && input.[i..i+2] = "!B=" ->
                    tokens <- NOT_EQ :: tokens; i <- i + 3
                | '>' when i + 1 < input.Length && input.[i+1] = 'B' ->
                    tokens <- GREATER :: tokens; i <- i + 2
                | '<' when i + 1 < input.Length && input.[i+1] = 'B' ->
                    tokens <- LESS :: tokens; i <- i + 2
                | 'B' when i + 1 < input.Length && input.[i+1] = '=' ->
                    tokens <- EQUALS :: tokens; i <- i + 2
                | '>' when i + 2 <= input.Length && input.[i..i+2] = ">V<" ->
                    tokens <- CONS :: tokens; i <- i + 3
                | '[' when i + 1 < input.Length && input.[i+1] = ']' ->
                    tokens <- NIL :: tokens; i <- i + 2
                | 'h' when i + 3 < input.Length && input.[i..i+3] = "head" ->
                    tokens <- HEAD :: tokens; i <- i + 4
                | 't' when i + 3 < input.Length && input.[i..i+3] = "tail" ->
                    tokens <- TAIL :: tokens; i <- i + 4
                | '(' -> tokens <- LPAREN :: tokens; i <- i + 1
                | ')' -> tokens <- RPAREN :: tokens; i <- i + 1
                | '_' when i + 1 < input.Length && input.[i+1] = '_' ->
                    tokens <- UNDERSCORE :: tokens; i <- i + 2
                | '"' ->
                    i <- i + 1
                    let s = readWhile (fun ch -> ch <> '"')
                    if i < input.Length then i <- i + 1
                    tokens <- STR s :: tokens
                | c when System.Char.IsDigit c ->
                    let n = readWhile System.Char.IsDigit
                    tokens <- INT (int n) :: tokens
                | c when System.Char.IsLetter c || c = '_' ->
                    let id = readWhile (fun ch -> System.Char.IsLetterOrDigit ch || ch = '_')
                    tokens <- ID id :: tokens
                | _ -> failwithf "Unexpected character: %c" c
        
        tokens <- List.rev tokens
        tokens @ [EOF]