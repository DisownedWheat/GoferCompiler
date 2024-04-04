open Core

let () =
  let _ = print_endline "Hello World" in
  match Ocaml_compiler.Lexer.lexInputFile "./test_file" with
  | Error x ->
    let _ = print_endline (fst x |> Ocaml_compiler.Lexer.show_token_type) in
    print_endline @@ snd x
  | Ok x ->
    x
    |> List.map ~f:Ocaml_compiler.Lexer.show_token_type
    |> List.map ~f:print_endline
    |> ignore
;;
