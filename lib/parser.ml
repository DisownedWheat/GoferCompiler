open Core

type root = ast_node list [@@deriving show]

and go_import =
  { module_name : string
  ; alias : string option
  }
[@@deriving show]

and logic_block = ast_node list [@@deriving show]
and gofer_import = { module_name : string } [@@deriving show]
and record_field = identifier * type_declaration [@@deriving show]
and record_definition = { fields : record_field list } [@@deriving show]
and record_literal = (string * ast_node) list [@@deriving show]

and identifier =
  { name : string
  ; mutable' : bool
  ; public : bool
  }
[@@deriving show]

and typed_identifier =
  { name : string
  ; type' : type_declaration
  ; mutable' : bool
  ; public : bool
  }
[@@deriving show]

and let_identifier_type =
  | Identifier of identifier
  | TypedIdentifier of typed_identifier
  | ArrayDestructure of identifier list
  | RecordDestructure of identifier list
  | TupleDestructure of identifier list
[@@deriving show]

and type_declaration =
  { name : string
  ; module' : string option
  ; pointer : bool
  ; slice : bool
  }
[@@deriving show]

and let_definition =
  { left : let_identifier_type
  ; right : ast_node
  }
[@@deriving show]

and functionArg =
  | ScopedArg of
      { name : string
      ; type' : string
      ; scoped_name : string
      }
  | TypedArg of typed_identifier
  | Arg of identifier
  | Null
[@@deriving show]

and function_def =
  { name : string option
  ; args : functionArg list
  ; body : logic_block
  ; struct' : typed_identifier option
  ; public : bool
  }
[@@deriving show]

and ast_node =
  | Root of root
  | GoImport of go_import
  | Import of gofer_import
  | FuncDef of function_def
  | Let of let_definition
  | Block of logic_block
  | TypeDefinition of string * ast_node
  | ParenExpression of ast_node
  | ArrayLiteral of ast_node list
  | RecordType of record_definition
  | RecordLiteral of record_literal
  | PubDeclaration of ast_node
  | NoOp
[@@deriving show]

type parse_delimiter =
  | FuncDelim of (Lexer.token_type list -> bool)
  | NoDelimiter

let print_ast ast =
  let rec print_ast' ast =
    match ast with
    | Root x -> List.iter ~f:print_ast' x
    | GoImport x ->
      Stdio.print_endline @@ Printf.sprintf "GoImport: %s" (show_go_import x)
    | Import x -> Stdio.print_endline @@ Printf.sprintf "Import: %s" (show_gofer_import x)
    | FuncDef x ->
      Stdio.print_endline @@ Printf.sprintf "FuncDef: %s" (show_function_def x)
    | Let x -> Stdio.print_endline @@ Printf.sprintf "Let: %s" (show_let_definition x)
    | Block x -> Stdio.print_endline @@ Printf.sprintf "Block: %s" (show_logic_block x)
    | TypeDefinition (x, _) ->
      Stdio.print_endline @@ Printf.sprintf "TypeDefinition: %s" x
    | ParenExpression x ->
      Stdio.print_endline @@ Printf.sprintf "ParenExpression: %s" (show_ast_node x)
    | ArrayLiteral x -> List.map ~f:show_ast_node x |> ignore
    | RecordType x ->
      Stdio.print_endline @@ Printf.sprintf "RecordType: %s" (show_record_definition x)
    | RecordLiteral x ->
      Stdio.print_endline @@ Printf.sprintf "RecordLiteral: %s" (show_record_literal x)
    | PubDeclaration x -> print_ast' x
    | NoOp -> Stdio.print_endline "NoOp"
  in
  print_ast' ast
;;

let ( ||> ) (a, b) f = f a b

let filter_noop x =
  match x with
  | NoOp -> false
  | _ -> true
;;

let debug_print_tokens tokens =
  List.map ~f:Lexer.show_token_type tokens |> List.map ~f:Stdio.print_endline
;;

let unexpected_token_error error_msg token =
  match token with
  | Some token ->
    let str =
      Printf.sprintf "Unexpected token: %s\n %s" error_msg (Lexer.show_token_type token)
    in
    Error str
  | None -> Error error_msg
;;

let rec ignore_whitespace tokens =
  match tokens with
  | [] -> tokens
  | Lexer.NewLine _ :: tail -> ignore_whitespace tail
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
      (fun a ->
        match a with
        | Lexer.RParen _ :: _ -> true
        | _ -> false)
  in
  match parse_tree delim tail' with
  | Ok (remaining, children) -> Ok (remaining, ParenExpression children)
  | Error e -> Error e

and parse_brace_expression tail =
  let delim =
    FuncDelim
      (fun a ->
        match a with
        | Lexer.RBrace _ :: _ -> true
        | _ -> false)
  in
  match rec_parse_tree delim tail [] with
  | Ok (remaining, children) -> Ok (remaining, children)
  | Error e -> Error e

and parse_record tail properties =
  match ignore_whitespace tail with
  | [] -> Ok (tail, RecordLiteral properties)
  | Lexer.RBrace _ :: _ -> Ok (tail, RecordLiteral properties)
  | Lexer.Identifier x :: Lexer.Colon _ :: remaining ->
    match_token remaining
    |> (fun result ->
         match result with
         | Ok (leftover, y) -> Ok (leftover, (x.value, y) :: properties)
         | Error e -> Error e)
    |> fun x ->
    (match x with
     | Error e -> Error e
     | Ok (leftover, y) ->
       (match leftover with
        | Lexer.Comma _ :: tail -> parse_record tail y
        | Lexer.RBrace _ :: _ -> parse_record leftover y
        | _ -> Error "Invalid record"))
  | _ ->
    unexpected_token_error "Invalid token when parsing record"
    @@ List.hd
    @@ ignore_whitespace tail

and parse_record_type tail properties =
  let check_for_comma previous_was_comma property =
    match previous_was_comma, property with
    | false, Some _ -> true
    | _ -> false
  in
  let rec parse' previous_was_comma tail properties =
    List.map ~f:show_record_field properties |> List.map ~f:print_endline |> ignore;
    match ignore_whitespace tail with
    | [] -> Ok (tail, List.rev properties)
    | Lexer.NewLine _ :: remaining -> parse' previous_was_comma remaining properties
    | Lexer.Comma _ :: remaining ->
      (*Handle the comma at the end of the property*)
      if check_for_comma previous_was_comma (List.hd properties)
      then parse' true remaining properties
      else unexpected_token_error "Invalid comma when parsing record type" @@ List.hd tail
    | Lexer.RBrace _ :: remaining -> Ok (remaining, List.rev properties)
    | Lexer.Pub _ :: Lexer.Identifier x :: Lexer.Colon _ :: remaining ->
      (match parse_type_literal @@ ignore_whitespace remaining with
       | Ok (remaining_tokens, t) ->
         parse'
           false
           remaining_tokens
           (({ name = x.value; mutable' = false; public = true }, t) :: properties)
       | Error e -> Error e)
    | Lexer.Pub _ :: Lexer.Mut _ :: Lexer.Identifier x :: Lexer.Colon _ :: remaining ->
      (match parse_type_literal @@ ignore_whitespace remaining with
       | Ok (remaining_tokens, t) ->
         parse'
           false
           remaining_tokens
           (({ name = x.value; mutable' = true; public = true }, t) :: properties)
       | Error e -> Error e)
    | Lexer.Mut _ :: Lexer.Identifier x :: Lexer.Colon _ :: remaining ->
      (match parse_type_literal @@ ignore_whitespace remaining with
       | Ok (remaining_tokens, t) ->
         parse'
           false
           remaining_tokens
           (({ name = x.value; public = false; mutable' = true }, t) :: properties)
       | Error e -> Error e)
    | Lexer.Identifier x :: Lexer.Colon _ :: remaining ->
      (match parse_type_literal @@ ignore_whitespace remaining with
       | Ok (remaining_tokens, t) ->
         parse'
           false
           remaining_tokens
           (({ name = x.value; public = false; mutable' = false }, t) :: properties)
       | Error e -> Error e)
    | _ ->
      unexpected_token_error "Invalid token when parsing record type"
      @@ List.hd
      @@ ignore_whitespace tail
  in
  parse' false tail properties

and parse_type_literal tail =
  let is_slice, tail' =
    match tail with
    | Lexer.LBracket _ :: Lexer.RBracket _ :: tail' -> true, tail'
    | _ -> false, tail
  in
  let is_pointer, tail'' =
    match tail' with
    | Lexer.Deref _ :: tail'' -> true, tail''
    | _ -> false, tail'
  in
  match tail'' with
  | Lexer.Identifier x :: Lexer.Dot _ :: Lexer.Identifier y :: tail ->
    Ok
      ( tail
      , { name = x.value; module' = Some y.value; pointer = is_pointer; slice = is_slice }
      )
  | Lexer.Identifier x :: tail ->
    Ok (tail, { name = x.value; module' = None; pointer = is_pointer; slice = is_slice })
  | _ -> unexpected_token_error "Invalid token when parsing type literal" @@ List.hd tail

and parse_type_dec ident tail =
  match ignore_whitespace tail with
  | Lexer.LBrace _ :: new_tail ->
    (match parse_record_type new_tail [] with
     | Ok (remaining, props) ->
       Ok (remaining, TypeDefinition (ident, RecordType { fields = props }))
     | Error e -> Error e)
  | _ -> Error "parse_type_dec Not Implemented"

and parse_array_literal tail =
  let delim =
    FuncDelim
      (fun a ->
        match a with
        | Lexer.RBracket _ :: _ -> true
        | _ -> false)
  in
  match rec_parse_tree delim tail [] with
  | Ok (remaining, children) -> Ok (remaining, ArrayLiteral children)
  | Error e -> Error e

and parse_struct_method_dec tail =
  match tail with
  | Lexer.Identifier x :: remaining ->
    let t = parse_type_literal @@ ignore_whitespace remaining in
    (match t with
     | Ok (remaining, t) ->
       Ok (remaining, { name = x.value; type' = t; public = false; mutable' = false })
     | Error e -> Error e)
  | _ -> unexpected_token_error "Invalid token when parsing struct method" @@ List.hd tail

and parse_function_args tail =
  let rec parse' previous_was_comma tail args =
    match tail with
    | [] -> Ok (tail, args)
    | Lexer.RParen _ :: tail -> Ok (tail, args)
    | Lexer.Comma _ :: tail ->
      if previous_was_comma
      then unexpected_token_error "Unexpected comma in function args" @@ List.hd tail
      else parse' true tail args
    | Lexer.Mut _ :: Lexer.Identifier x :: Lexer.Colon _ :: tail ->
      (match parse_type_literal @@ ignore_whitespace tail with
       | Ok (remaining, t) ->
         parse'
           false
           remaining
           (TypedArg { name = x.value; type' = t; public = false; mutable' = true }
            :: args)
       | Error e -> Error e)
    | Lexer.Identifier x :: Lexer.Colon _ :: tail ->
      (match parse_type_literal @@ ignore_whitespace tail with
       | Ok (remaining, t) ->
         parse'
           false
           remaining
           (TypedArg { name = x.value; type' = t; public = false; mutable' = false }
            :: args)
       | Error e -> Error e)
    | Lexer.Mut _ :: Lexer.Identifier x :: tail ->
      parse' false tail (Arg { name = x.value; public = false; mutable' = true } :: args)
    | Lexer.Identifier x :: tail ->
      parse' false tail (Arg { name = x.value; public = false; mutable' = false } :: args)
    | _ ->
      unexpected_token_error "Invalid token when parsing function args" @@ List.hd tail
  in
  parse' false tail []

and parse_function ?(public = false) tail =
  let rec parse' is_struct_method tail' =
    match tail' with
    | Lexer.LParen _ :: tail' ->
      let function_args = parse_function_args tail' in
      let struct_method = parse_struct_method_dec tail' in
      (match function_args, struct_method with
       | Ok _, Ok _ ->
         Error
           "Inconclusive function args, not sure whether a struct method or anonymous \
            function"
       | Ok (tail, args), _ ->
         (match parse_brace_expression tail with
          | Ok (remaining, body) ->
            Ok (remaining, FuncDef { name = None; args; body; struct' = None; public })
          | Error e -> Error e)
       | _, Ok (remaining, args) -> parse' (Some args) remaining
       | _ -> unexpected_token_error "Invalid function args" @@ List.hd tail')
    | Lexer.Identifier x :: Lexer.LParen _ :: tail' ->
      let function_args = parse_function_args tail' in
      (match function_args with
       | Ok (tail, args) ->
         (match parse_brace_expression tail with
          | Ok (remaining, body) ->
            Ok
              ( remaining
              , FuncDef
                  { name = Some x.value; args; body; struct' = is_struct_method; public }
              )
          | Error e -> Error e)
       | Error e -> Error e)
    | _ -> unexpected_token_error "Invalid token when parsing function" @@ List.hd tail
  in
  parse' None tail

and unpack_assignment tail left =
  let head = List.hd tail in
  let tail' = List.tl tail in
  match head, tail' with
  | Some (Lexer.Assign _), Some tail -> Ok (tail, List.rev left)
  | Some x, Some tail -> unpack_assignment tail (x :: left)
  | _ -> unexpected_token_error "Invalid unpacking of left-right assignment" head

and parse_let_expression tail =
  Stdio.print_endline "";
  List.map ~f:Lexer.show_token_type tail |> List.map ~f:Stdio.print_endline |> ignore;
  let rec parse_array_destructure tail' args =
    match tail' with
    | Lexer.Identifier x :: Lexer.Comma _ :: remaining ->
      parse_array_destructure
        remaining
        ({ name = x.value; mutable' = false; public = false } :: args)
    | Lexer.Identifier x :: Lexer.RBracket _ :: Lexer.Assign _ :: remaining ->
      Ok
        ( remaining
        , ArrayDestructure ({ name = x.value; public = false; mutable' = false } :: args)
        )
    | _ -> unexpected_token_error "Invalid destructure" @@ List.hd tail'
  in
  let rec parse_tuple_destructre tail' args =
    match tail with
    | Lexer.Identifier x :: Lexer.Comma _ :: remaining ->
      parse_tuple_destructre
        remaining
        ({ name = x.value; mutable' = false; public = false } :: args)
    | Lexer.Identifier x :: Lexer.RParen _ :: remaining ->
      Ok
        ( remaining
        , RecordDestructure ({ name = x.value; public = false; mutable' = false } :: args)
        )
    | _ -> unexpected_token_error "Invalid destructure" @@ List.hd tail'
  in
  let rec parse_record_destructre tail' args =
    match tail with
    | Lexer.Identifier x :: Lexer.Comma _ :: remaining ->
      parse_record_destructre
        remaining
        ({ name = x.value; mutable' = false; public = false } :: args)
    | Lexer.Identifier x :: Lexer.RBrace _ :: Lexer.Assign _ :: remaining ->
      Ok
        ( remaining
        , RecordDestructure ({ name = x.value; public = false; mutable' = false } :: args)
        )
    | _ -> unexpected_token_error "Invalid destructure" @@ List.hd tail'
  in
  let rec parse_left ?(mutable' = false) ?(public = false) tail =
    match tail with
    | Lexer.LBrace _ :: tail -> parse_record_destructre tail []
    | Lexer.LBracket _ :: tail -> parse_array_destructure tail []
    | Lexer.LParen _ :: tail -> parse_tuple_destructre tail []
    | Lexer.Pub _ :: tail -> parse_left ~mutable' ~public:true tail
    | Lexer.Mut _ :: tail -> parse_left ~mutable':true ~public tail
    | Lexer.Identifier x :: Lexer.Colon _ :: tail ->
      (match parse_type_literal tail with
       | Ok (remaining, type') ->
         Ok (remaining, TypedIdentifier { name = x.value; type'; mutable'; public })
       | Error e -> Error e)
    | Lexer.Identifier x :: _ -> Ok ([], Identifier { name = x.value; public; mutable' })
    | _ -> unexpected_token_error "Invalid token when parsing let" @@ List.hd tail
  in
  match unpack_assignment tail [] with
  | Ok (right, left) ->
    Stdio.print_endline "LEFT";
    List.map ~f:Lexer.show_token_type left |> List.map ~f:Stdio.print_endline |> ignore;
    Stdio.print_endline "RIGHT";
    List.map ~f:Lexer.show_token_type right |> List.map ~f:Stdio.print_endline |> ignore;
    let parsed_left = parse_left left in
    let parsed_right = match_token right in
    (match parsed_left, parsed_right with
     | Ok (_, left), Ok (remaining', right) -> Ok (remaining', Let { left; right })
     | Error e, _ -> Error e
     | _, Error e -> Error e)
  | Error e -> Error e

and parse_identifier token tail =
  Stdio.print_endline @@ Lexer.show_token token;
  match tail with
  | _ -> Error "Parsing identifier not implemented"

and parse_logic_block tail =
  let delim =
    FuncDelim
      (fun a ->
        match a with
        | Lexer.RBrace _ :: _ -> true
        | _ -> false)
  in
  match rec_parse_tree delim tail [] with
  | Ok (remaining, children) -> Ok (remaining, Block children)
  | Error e -> Error e

and wrap_pub tail =
  match match_token tail with
  | Ok (remaining, x) -> Ok (remaining, PubDeclaration x)
  | Error e -> Error e

and match_token tail' =
  match tail' with
  | Lexer.EOF _ :: tail -> Ok (tail, NoOp)
  | Lexer.Import _ :: tail ->
    (match parse_import tail with
     | Ok (x, y) -> Ok (x, y)
     | Error e -> Error e)
  | Lexer.NewLine _ :: tail -> Ok (tail, NoOp)
  | Lexer.LBracket _ :: tail ->
    (match parse_array_literal tail with
     | Ok (remaining, node) -> Ok (remaining, node)
     | Error e -> Error e)
  | Lexer.LParen _ :: tail -> parse_paren_expression tail
  | Lexer.LBrace _ :: tail ->
    (match ignore_whitespace tail with
     | Lexer.Identifier _ :: Lexer.Colon _ :: _ ->
       (match parse_record tail [] with
        | _ -> Ok (tail, NoOp))
     | _ -> parse_logic_block tail)
  | Lexer.Pub _ :: tail -> wrap_pub tail
  | Lexer.Function _ :: tail -> parse_function tail
  | Lexer.Let _ :: tail -> parse_let_expression tail
  | Lexer.Identifier x :: tail -> parse_identifier x tail
  | Lexer.TypeKeyword _ :: tail ->
    (match ignore_whitespace tail with
     | Lexer.Identifier x :: Lexer.Assign _ :: tail -> parse_type_dec x.value @@ tail
     | _ -> unexpected_token_error "Unmatched Token" @@ List.hd tail)
  | _ -> unexpected_token_error "Unmatched Token" @@ List.hd tail'

and parse_tree delimiter tokens =
  match delimiter, tokens with
  | _, [] -> Ok (tokens, NoOp)
  | FuncDelim f, tail when not (f tail) -> match_token tail
  | NoDelimiter, tail -> match_token tail
  | _, _ :: tail -> Ok (tail, NoOp)

and rec_parse_tree delimiter tokens children =
  match delimiter, tokens with
  | FuncDelim f, tail when not (f tail) ->
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
  match tokens with
  | [] -> Ok (tokens, List.filter ~f:filter_noop ast_list |> List.rev)
  | _ ->
    (match parse_tree NoDelimiter tokens with
     | Ok (tokens, child) -> recursive_parse tokens (child :: ast_list)
     | Error y -> Error y)
;;

let parse tokens =
  (* debug_print_tokens tokens; *)
  match recursive_parse tokens [] with
  | Error e -> Error e
  | Ok (_, ast) -> Ok (Root ast)
;;
