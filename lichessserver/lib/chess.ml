open Angstrom
open Base

let integer =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
;;

let parse_int = lift (Printf.sprintf "%s") integer
let add = char '+'
let minus = char '-'
let op = add <|> minus

let expr =
  fix (fun expr ->
    let two =
      lift3 (fun a op b -> Printf.sprintf "(%c %s %s )" op a b) integer op integer
    in
    let more = lift3 (fun a op b -> Printf.sprintf "(%c %s %s)" op a b) integer op expr in
    more <|> two <|> parse_int)
;;

let%expect_test "number" =
  let number = "10" in
  match parse_string ~consume:All parse_int number with
  | Ok number ->
    Stdio.printf "%s" number;
    [%expect {|10|}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "a plus b" =
  let text = "1+2" in
  match parse_string ~consume:All expr text with
  | Ok expression ->
    Stdio.printf "%s" expression;
    [%expect {| (+ 1 2) |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "a plus b plus c" =
  let text = "1+2+3" in
  match parse_string ~consume:All expr text with
  | Ok expression ->
    Stdio.printf "%s" expression;
    [%expect {| (+ 1 (+ 2 3)) |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "a plus b plus c plus d" =
  let text = "1+2+3+4" in
  match parse_string ~consume:All expr text with
  | Ok expression ->
    Stdio.printf "%s" expression;
    [%expect {| (+ 1 (+ 2 (+ 3 4))) |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "a plus b plus c minus d" =
  let text = "1+2+3-4" in
  match parse_string ~consume:All expr text with
  | Ok expression ->
    Stdio.printf "%s" expression;
    [%expect {| (+ 1 (+ 2 (- 3 4))) |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

type element =
  { word : string
  ; info : string option
  }

let is_word_char = function
  | ' ' | '(' | ')' | ',' -> false
  | _ -> true
;;

let word = take_while1 is_word_char

let ws =
  skip_many
    (satisfy (function
       | ' ' | '\t' | '\n' -> true
       | _ -> false))
;;

let parens_info =
  option
    None
    (ws *> char '(' *> ws *> take_while (fun c -> not (Char.equal c ')'))
     <* ws
     <* char ')'
     >>| fun s -> Some s)
;;

let element = lift2 (fun w info -> { word = w; info }) (ws *> word <* ws) parens_info
let comma_sep = ws *> char ',' *> ws
let and_sep = ws *> string "und" *> ws

let elements =
  sep_by1 comma_sep element
  >>= fun list1 -> and_sep *> element >>| (fun last -> list1 @ [ last ]) <|> return list1
;;

let%expect_test "ice cream single" =
  let text = {|Vanille|} in
  match parse_string ~consume:All elements text with
  | Ok ice_list ->
    List.iter ice_list ~f:(fun el ->
      match el.info with
      | Some i -> Stdio.printf "%s info:%s\n" el.word i
      | None -> Stdio.printf "%s no info\n" el.word);
    [%expect {| Vanille no info |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "ice cream list" =
  let text =
    {|Vanille, Schokolade, Mango (vegan), Joghurt, Erdbeere (vegan), Walnuss, Zitrone (vegan), Milchreis, Aprikose (vegan), KaBuBa, Schwarze-Johannisbeere (vegan), Kokos und Ananas-Curry (vegan)|}
  in
  match parse_string ~consume:All elements text with
  | Ok ice_list ->
    List.iter ice_list ~f:(fun el ->
      match el.info with
      | Some i -> Stdio.printf "%s info:%s\n" el.word i
      | None -> Stdio.printf "%s no info\n" el.word);
    [%expect
      {|
      Vanille no info
      Schokolade no info
      Mango info:vegan
      Joghurt no info
      Erdbeere info:vegan
      Walnuss no info
      Zitrone info:vegan
      Milchreis no info
      Aprikose info:vegan
      KaBuBa no info
      Schwarze-Johannisbeere info:vegan
      Kokos no info
      Ananas-Curry info:vegan
      |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;
