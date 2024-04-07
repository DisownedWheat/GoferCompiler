open Core

type root = ast_node list

and go_import =
  { module_name : string
  ; alias : string option
  }

and logic_block = ast_node list
and gofer_import = { module_name : string }
and record_field = string * type_declaration
and record_definition = { fields : record_field list }
and record_declaration = string * ast_node
and identifier = { name : string }

and typed_identifier =
  { name : string
  ; type' : type_declaration
  }

and identifier_type =
  | Identifier of identifier
  | TypedIdentifier of typed_identifier

and type_declaration =
  { name : string
  ; module' : string option
  ; pointer : bool
  }

and let_definition =
  { left : identifier_type
  ; right : ast_node
  }

and functionArg =
  | ScopedArg of
      { name : string
      ; type' : string
      ; scoped_name : string
      }
  | TypedArg of typed_identifier
  | Arg of identifier
  | Null

and function_def =
  { name : string option
  ; args : functionArg list
  ; body : logic_block
  ; pointer : identifier option
  }

and ast_node =
  | Root of root
  | GoImport of go_import
  | Import of gofer_import
  | FuncDef of function_def
  | Let of let_definition
  | Block of logic_block
  | ParenExpression of ast_node
  | ArrayLiteral of ast_node list
  | RecordType of identifier * record_definition
  | RecordLiteral of record_declaration
  | NoOp
[@@deriving show]

type parse_delimiter =
  | FuncDelim of (Lexer.token_type -> Lexer.token_type list -> bool)
  | NoDelimiter

let ( ||> ) (a, b) f = f a b

let filter_noop x =
  match x with
  | NoOp -> false
  | _ -> true
;;

let debug_print_tokens tokens =
  List.map ~f:Lexer.show_token_type tokens |> List.map ~f:Stdio.print_endline
;;

let unexpected_token_error token =
  match token with
  | Some token ->
    let str = Printf.sprintf "Unexpected token: %s" (Lexer.show_token_type token) in
    Error str
  | None -> Error "Invalid token"
;;

(* |> fun _ -> Stdio.print_endline "Done" *)

let rec ignore_whitespace ignore_newlines tokens =
  match tokens, ignore_newlines with
  | [], _ -> tokens
  | Lexer.NewLine _ :: tail, true -> ignore_whitespace ignore_newlines tail
  | Lexer.Whitespace _ :: tail, _ -> ignore_whitespace ignore_newlines tail
  | _ -> tokens
;;

let rec parse_import tail' =
  match tail' with
  | Lexer.String x :: Lexer.NewLine _ :: tail ->
    Ok (tail, GoImport { module_name = x.value; alias = None })
  | Lexer.Identifier x :: Lexer.String y :: Lexer.NewLine _ :: tail ->
    Ok (tail, GoImport { module_name = x.value; alias = Some y.value })
  | Lexer.Identifier x :: Lexer.NewLine _ :: tail ->
    Ok (tail, Import { module_name = x.value })
  | _ -> Error "Invalid import line"

and parse_paren_expression tail' =
  let delim =
    FuncDelim
      (fun a _ ->
        match a with
        | Lexer.RParen _ -> true
        | _ -> false)
  in
  match parse_tree delim tail' with
  | Ok (remaining, children) -> Ok (remaining, ParenExpression children)
  | Error e -> Error e

and parse_brace_expression tail =
  let delim =
    FuncDelim
      (fun a _ ->
        match a with
        | Lexer.RBrace _ -> true
        | _ -> false)
  in
  match rec_parse_tree delim tail [] with
  | Ok (remaining, children) -> Ok (remaining, Block children)
  | Error e -> Error e

and parse_record tail properties =
  match ignore_whitespace true tail with
  | [] -> Ok (tail, properties)
  | Lexer.RBrace _ :: _ -> Ok (tail, properties)
  | Lexer.Identifier x :: Lexer.Colon _ :: head :: remaining ->
    (match match_token head remaining with
     | Ok (leftover, y) -> parse_record leftover ((x.value, y) :: properties)
     | Error e -> Error e)
  | _ -> unexpected_token_error @@ List.hd tail

and parse_record_type tail properties =
  match ignore_whitespace true tail with
  | [] -> Ok (tail, properties)
  | Lexer.RBrace _ :: remaining -> Ok (remaining, properties)
  | Lexer.Identifier _ :: Lexer.Colon _ :: remaining ->
    (match parse_type_literal remaining with
     | Ok (remaining_tokens, _) -> parse_record_type remaining_tokens []
     | Error e -> Error e)
  | _ -> Error "parse_record_type Not implemented yet"

and parse_type_literal tail =
  match tail with
  | Lexer.Identifier x :: Lexer.Dot _ :: Lexer.Identifier y :: tail ->
    Ok (tail, { name = x.value; module' = Some y.value; pointer = false })
  | _ -> Error "parse_type_literal Not Implemented"

and parse_type_dec ident tail =
  match ignore_whitespace true tail with
  | Lexer.LBrace _ :: new_tail ->
    (match parse_record_type new_tail [] with
     | Ok (remaining, props) -> Ok (remaining, RecordType (ident, { fields = props }))
     | Error e -> Error e)
  | _ -> Error "parse_type_dec Not Implemented"

and parse_array_literal tail =
  let delim =
    FuncDelim
      (fun a _ ->
        match a with
        | Lexer.RBracket _ -> true
        | _ -> false)
  in
  match rec_parse_tree delim tail [] with
  | Ok (remaining, children) -> Ok (remaining, ArrayLiteral children)
  | Error e -> Error e

and match_token head tail =
  match head with
  | Lexer.EOF _ -> Ok (tail, NoOp)
  | Lexer.Import _ ->
    (match parse_import tail with
     | Ok (x, y) -> Ok (x, y)
     | Error e -> Error e)
  | Lexer.NewLine _ -> Ok (tail, NoOp)
  | Lexer.LBracket _ ->
    (match parse_array_literal tail with
     | Ok (remaining, node) -> Ok (remaining, node)
     | Error e -> Error e)
  | Lexer.LParen _ -> parse_paren_expression tail
  | Lexer.LBrace _ ->
    (match ignore_whitespace true tail with
     | Lexer.Identifier _ :: Lexer.Colon _ :: _ ->
       (match parse_record tail [] with
        | _ -> Ok (tail, NoOp))
     | _ -> Ok (tail, NoOp))
  | Lexer.TypeKeyword _ ->
    (match ignore_whitespace true tail with
     | Lexer.Identifier x :: tail -> parse_type_dec { name = x.value } tail
     | _ -> unexpected_token_error @@ Some head)
  | _ -> unexpected_token_error @@ Some head

and parse_tree delimiter tokens =
  match delimiter, tokens with
  | _, [] -> Ok (tokens, NoOp)
  | FuncDelim f, head :: tail when not (f head tokens) -> match_token head tail
  | NoDelimiter, head :: tail -> match_token head tail
  | _, _ :: tail -> Ok (tail, NoOp)

and rec_parse_tree delimiter tokens children =
  match delimiter, tokens with
  | FuncDelim f, head :: _ when not (f head tokens) ->
    (match parse_tree delimiter tokens with
     | Ok (remaining, child) -> rec_parse_tree delimiter remaining (child :: children)
     | Error e -> Error e)
  | NoDelimiter, _ ->
    (match parse_tree delimiter tokens with
     | Ok (remaining, child) -> rec_parse_tree delimiter remaining (child :: children)
     | Error e -> Error e)
  | _, _ ->
    let filtered = List.filter ~f:filter_noop children |> List.rev in
    Ok (tokens, filtered)

and recursive_parse tokens ast_list =
  let _ = debug_print_tokens tokens in
  match tokens with
  | [] -> Ok (tokens, List.filter ~f:filter_noop ast_list |> List.rev)
  | _ ->
    (match parse_tree NoDelimiter tokens with
     | Ok (tokens, child) -> recursive_parse tokens (child :: ast_list)
     | Error y -> Error y)
;;

let parse tokens =
  match recursive_parse tokens [] with
  | Error e -> Error e
  | Ok (_, ast) -> Ok (Root ast)
;;
