open Base

let () =
  let _ = Stdio.print_endline "Hello World" in
  match Ocaml_compiler.Lexer.lexInputFile "./test_file" with
  | Error x ->
    let _ = Stdio.print_endline (fst x |> Ocaml_compiler.Lexer.show_token_type) in
    Stdio.print_endline @@ snd x
  | Ok x ->
    let parsed = Ocaml_compiler.Parser.parse x in
    (match parsed with
     | Error e -> Stdio.print_endline e
     | Ok (Root x) ->
       x
       |> List.map ~f:Ocaml_compiler.Parser.show_ast_node
       |> List.map ~f:Stdio.print_endline
       |> ignore
     | _ -> Stdio.print_endline "Invalid")
;;
