open Printf
open Expr

type result =
  | OK of string
  | Fail of string option

let fail = Fail None
let error msg = Fail (Some msg)

let string_of_result = function
  | Fail None -> "Fail"
  | Fail (Some msg) -> "Fail " ^ msg
  | OK ty_str -> "OK " ^ ty_str

let normalize ty_str = string_of_ty (Parser.ty_forall_eof Lexer.token (Lexing.from_string ty_str))

let eval_type code =
  let result =
    try
      Infer.reset_id () ;
      let ty = Infer.infer Core.core 0 (Parser.expr_eof Lexer.token (Lexing.from_string code)) in
      let generalized_ty = Infer.generalize (-1) ty in
      OK (string_of_ty generalized_ty)
    with Infer.Error msg ->
      Fail (Some msg)
  in
  printf "%s\n" (string_of_result result)

let () =
  let input = (List.fold_left (fun acc x -> acc ^ " " ^ x) "" (List.tl (Array.to_list Sys.argv))) in
  eval_type input