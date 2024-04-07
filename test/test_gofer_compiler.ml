open Core

let () =
  let _ = Stdio.print_endline "Hello World" in
  match Gofer_compiler.Lexer.lexInputFile "./test_file" with
  | Error x ->
    let _ = Stdio.print_endline (fst x |> Gofer_compiler.Lexer.show_token_type) in
    Stdio.print_endline @@ snd x
  | Ok x ->
    let parsed = Gofer_compiler.Lparser.parse x in
    (match parsed with
     | Error e -> Stdio.print_endline e
     | Ok x -> Gofer_compiler.Lparser.print_ast x)
;;
(* | Ok (Root x) -> *)
(*   x *)
(*   |> List.map ~f:Gofer_compiler.Parser.show_ast_node *)
(*   |> List.map ~f:Stdio.print_endline *)
(*   |> ignore *)
(* | _ -> Stdio.print_endline "Invalid") *)

