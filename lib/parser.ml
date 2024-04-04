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

type parse_delimiter =
  | FuncDelim of (Lexer.token_type -> Lexer.token_type list -> ast_node -> bool)
  | NoDelimiter

let ( ||> ) (a, b) f = f a b

let rec parse_tree tokens ast =
  match tokens with
  | [] -> Ok (tokens, ast)
  | head :: _ ->
    (match head with
     | Lexer.EOF _ -> Ok (tokens, ast)
     | _ -> Ok (tokens, ast))

and recursive_parse tokens ast =
  match tokens with
  | [] -> Ok (tokens, ast)
  | _ ->
    (match parse_tree tokens ast with
     | Ok x -> x ||> recursive_parse
     | Error y -> Error y)
;;
