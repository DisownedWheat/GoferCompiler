open Core

type token =
  { line : int
  ; column : int
  ; value : string
  }
[@@deriving show]

type token_type =
  | Number of token
  | Let of token
  | Import of token
  | String of token
  | Keyword of token
  | Go of token
  | If of token
  | Else of token
  | True of token
  | False of token
  | Return of token
  | Match of token
  | Operator of token
  | Identifier of token
  | Assign of token
  | LBrace of token
  | RBrace of token
  | LBracket of token
  | RBracket of token
  | Colon of token
  | LParen of token
  | RParen of token
  | Comma of token
  | Dot of token
  | Pointer of token
  | Deref of token
  | Channel of token
  | Append of token
  | PropertyAccess of token
  | NewLine of token
  | TypeKeyword of token
  | Pub of token
  | Mut of token
  | Function of token
  | Equality of token
  | GT of token
  | LT of token
  | GTE of token
  | LTE of token
  | EOF of token
  | And of token
  | Or of token
  | Pipe of token
  | TypeSeparator of token
  | Range of token
  | Comment of token
  | ReturnType of token
  | Whitespace of token
  | ErrorToken of token
[@@deriving show]

type lexerState =
  { line : int
  ; column : int
  ; tokens : token_type list
  ; buffer : char list
  }

type charFuncInput =
  | Token of (token -> token_type)
  | Char
  | Ignore of lexerState

let build_token state value = { line = state.line; column = state.column; value }

let build_token_char state value =
  { line = state.line; column = state.column; value = Char.to_string value }
;;

let id a = a
let ( >> ) f g x = g (f x)

let check_number chars =
  match chars with
  | [] -> false
  | head :: tail ->
    (match Char.is_digit head with
     | false -> false
     | true -> List.for_all ~f:(fun x -> Char.is_digit x || Char.equal x '_') tail)
;;

let parse_current state =
  let filtered = List.filter ~f:(Char.is_whitespace >> not) state.buffer in
  match filtered with
  | [] -> state
  | _ ->
    let value = String.of_char_list @@ List.rev filtered in
    let token = build_token state value in
    let t =
      match check_number filtered with
      | true -> Number (build_token state value)
      | false ->
        (match value with
         | "if" -> If token
         | "else" -> Else token
         | "let" -> Let token
         | "true" -> True token
         | "false" -> False token
         | "pub" -> Pub token
         | "go" -> Go token
         | "type" -> TypeKeyword token
         | "fn" -> Function token
         | "mut" -> Mut token
         | "return" -> Return token
         | "import" -> Import token
         | "match" -> Match token
         | _ -> Identifier token)
    in
    { state with
      buffer = []
    ; tokens = t :: state.tokens
    ; column = state.column + String.length value
    }
;;

let check_whitespace state char =
  let is_string =
    match List.last state.buffer with
    | Some '"' -> true
    | _ -> false
  in
  let parse_func =
    match is_string with
    | true -> id
    | false -> parse_current
  in
  let token_func state token =
    match is_string with
    | true -> state.tokens
    | false -> token :: state.tokens
  in
  let append_buffer_func this_acc =
    match is_string with
    | true -> char :: this_acc.buffer
    | false -> this_acc.buffer
  in
  let new_state = parse_func state in
  match char, is_string with
  | '\n', false | '\r', false ->
    { tokens = token_func new_state @@ NewLine (build_token_char new_state char)
    ; buffer = append_buffer_func new_state
    ; line = new_state.line + 1
    ; column = 1
    }
  | ' ', false -> { new_state with column = new_state.column + 1 }
  | '\t', false ->
    { new_state with
      tokens = token_func new_state @@ Whitespace (build_token_char new_state char)
    ; buffer = append_buffer_func new_state
    ; column = new_state.column + 4
    }
  | _ -> { state with buffer = char :: state.buffer }
;;

let char_token state char type' =
  match type' with
  | Token t ->
    Ok
      { state with
        tokens = t (Char.to_string char |> build_token state) :: state.tokens
      ; column = state.column + 1
      ; buffer = []
      }
  | Char -> Ok { state with buffer = char :: state.buffer }
  | Ignore x -> Ok x
;;

let match_char char state =
  let newState = parse_current state in
  char_token
    newState
    char
    (match char with
     | '+' | '%' | '^' | '~' -> Token (fun x -> Operator x)
     | ',' -> Token (fun x -> Comma x)
     | '(' -> Token (fun x -> LParen x)
     | ')' -> Token (fun x -> RParen x)
     | '{' -> Token (fun x -> LBrace x)
     | '}' -> Token (fun x -> RBrace x)
     | '[' -> Token (fun x -> LBracket x)
     | ']' -> Token (fun x -> RBracket x)
     | '@' -> Token (fun x -> PropertyAccess x)
     | '|' | '.' -> Char
     | _ -> Ignore (check_whitespace state char))
;;

let quick_append_token state length token =
  { state with
    buffer = []
  ; tokens = token :: state.tokens
  ; column = state.column + length
  }
;;

let ( |+> ) state (token, offset) = quick_append_token state offset token

let parseText state char =
  let last_token = List.last state.buffer in
  match last_token with
  | Some '"' ->
    (match char with
     | '"' ->
       let token =
         { line = state.line
         ; column = state.column
         ; value =
             (List.rev state.buffer
              |> List.tl
              |> fun x ->
              match x with
              | Some value -> String.of_char_list value
              | None -> "")
         }
       in
       Ok
         { state with
           tokens = String token :: state.tokens
         ; buffer = []
         ; column = state.column + String.length token.value + 2
         }
     | _ -> Ok { state with buffer = char :: state.buffer })
  | Some '.' ->
    (match char with
     | '.' -> Ok (state |+> (Range (build_token state ".."), 2))
     | _ -> state |+> (PropertyAccess (build_token state "."), 1) |> match_char char)
  | Some '=' ->
    (match char with
     | '=' -> Ok (state |+> (Equality (build_token state "=="), 2))
     | _ -> state |+> (Assign (build_token state "="), 1) |> match_char char)
  | Some '-' ->
    (match char with
     | '>' -> Ok (state |+> (ReturnType (build_token state "->"), 2))
     | _ -> state |+> (Operator (build_token state "-"), 1) |> match_char char)
  | Some '<' ->
    (match char with
     | '=' -> Ok (state |+> (LTE (build_token state "<="), 2))
     | '-' -> Ok (state |+> (Channel (build_token state "<-"), 2))
     | _ -> state |+> (LT (build_token state "<"), 1) |> match_char char)
  | Some '*' ->
    if Char.is_whitespace char
    then state |+> (Operator (build_token_char state '*'), 1) |> match_char char
    else state |+> (Deref (build_token_char state '*'), 1) |> match_char char
  | Some '>' ->
    (match char with
     | '=' -> Ok (state |+> (GTE (build_token state ">="), 2))
     | _ -> state |+> (GT (build_token state ">"), 1) |> match_char char)
  | Some '&' ->
    if Char.is_whitespace char
    then Error (ErrorToken (build_token state "&"), "Invalid & character")
    else (
      match char with
      | '&' -> Ok (state |+> (And (build_token state "&&"), 2))
      | _ -> state |+> (Pointer (build_token state "&"), 2) |> match_char char)
  | Some '|' ->
    (match char with
     | '|' -> Ok (state |+> (Or (build_token state "||"), 2))
     | '>' -> Ok (state |+> (Pipe (build_token state "|>"), 2))
     | _ -> state |+> (TypeSeparator (build_token state "|"), 1) |> match_char char)
  | Some ':' ->
    (match char with
     | ':' -> Ok (state |+> (Append (build_token state "::"), 2))
     | _ -> state |+> (Colon (build_token state ":"), 1) |> match_char char)
  | Some '/' ->
    (match char with
     | '/' -> Ok (state |+> (Comment (build_token state "//"), 2))
     | _ -> state |+> (Operator (build_token state "/"), 1) |> match_char char)
  | _ -> match_char char state
;;

let lex input =
  let rec lex' state input =
    match input with
    | [] ->
      (match List.length state.buffer with
       | 0 -> Ok state.tokens
       | _ ->
         let e_token =
           match List.hd state.tokens with
           | Some x -> x
           | None -> ErrorToken (build_token state "")
         in
         Error (e_token, "Unexpected EOF"))
    | c :: cs ->
      let state' = parseText state c in
      (match state' with
       | Ok x -> lex' x cs
       | Error e -> Error e)
  in
  lex' { line = 1; column = 1; tokens = []; buffer = [] } input
;;

let lexInputFile file_name =
  let input = In_channel.read_all file_name in
  let _ = print_endline input in
  match lex (String.to_list input) with
  | Ok x ->
    let tokens = EOF { line = 0; column = 0; value = "EOF" } :: x in
    Ok (List.rev tokens)
  | Error e -> Error e
;;
