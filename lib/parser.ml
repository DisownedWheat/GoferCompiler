open Base

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
  Error (Printf.sprintf "Unexpected token: %s" (Lexer.show_token_type token))
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
  let _ = Stdio.print_endline "Parsing import" in
  let check_import_type tokens =
    match tokens with
    | Lexer.String x :: Lexer.NewLine _ :: tail ->
      Ok (tail, GoImport { module_name = x.value; alias = None })
    | Lexer.Identifier x :: Lexer.String y :: Lexer.NewLine _ :: tail ->
      Ok (tail, GoImport { module_name = x.value; alias = Some y.value })
    | Lexer.Identifier x :: Lexer.NewLine _ :: tail ->
      Ok (tail, Import { module_name = x.value })
    | _ -> Error "Invalid import line"
  in
  match check_import_type tail' with
  | Error e -> Error e
  | Ok x -> Ok x

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

and match_token _ head tail =
  match head with
  | Lexer.EOF _ -> Ok (tail, NoOp)
  | Lexer.Import _ -> parse_import tail
  | Lexer.NewLine _ -> Ok (tail, NoOp)
  | Lexer.LBracket _ -> parse_array_literal tail
  | Lexer.LParen _ -> parse_paren_expression tail
  | Lexer.LBrace _ -> parse_brace_expression tail
  | _ -> unexpected_token_error head

and parse_tree delimiter tokens =
  let curried_parse = match_token delimiter in
  match delimiter, tokens with
  | _, [] -> Ok (tokens, NoOp)
  | FuncDelim f, head :: tail when not (f head tokens) -> curried_parse head tail
  | NoDelimiter, head :: tail -> curried_parse head tail
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
  | _, _ -> Ok (tokens, List.filter ~f:filter_noop children |> List.rev)

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
