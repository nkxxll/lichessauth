open Angstrom
open Base

type config =
  { key : string
  ; value : string
  }
[@@deriving sexp]

type token =
  | Number of int
  | Notation of string
  | LBracket
  | RBracket

let ws =
  skip_while (function
    | ' ' | '\n' | '\t' | '\r' -> true
    | _ -> false)
;;

let comment =
  choice
    [ char '{'
      *> skip_while (function
        | '}' -> false
        | _ -> true)
      <* char '}'
    ; char ';'
      *> skip_while (function
        | '\n' -> false
        | _ -> true)
    ]
;;

let not_relevant = choice [ ws; comment ]
let key = take_while1 Char.is_alpha
let value = char '"' *> take_while1 (fun c -> not (Char.equal '"' c)) <* char '"'

let config_pair =
  char '[' *> key
  >>= fun key -> not_relevant *> value <* char ']' >>| fun value -> { key; value }
;;

let number =
  take_while1 Char.is_digit <* char '.' >>| fun number -> Number (Int.of_string number)
;;

let notation = take_while1 Char.is_alphanum >>| fun not -> Notation not
let l_bracket = char '(' >>| fun _ -> LBracket
let r_bracket = char ')' >>| fun _ -> RBracket
let config = sep_by (char '\n') config_pair

let move_section =
  sep_by (many not_relevant) (choice [ number; notation; l_bracket; r_bracket ])
;;

let%expect_test "config one move" =
  let text =
    {|[Event "Test"]
[Site "London"]
[Date "2025.08.08"]
[Round "2"]
[White "Tom"]
[Black "Tim"]
[Result "*"]
[WhiteElo "2000"]
[BlackElo "2000"]
[TimeControl "10 "]
[Termination "*"]
[Link "https://www.chess.com/analysis"]|}
  in
  match parse_string ~consume:All config text with
  | Ok res ->
    res
    |> List.iter ~f:(fun c ->
      c |> sexp_of_config |> Sexp.to_string |> Stdio.print_endline);
    [%expect
      {|
      ((key Event)(value Test))
      ((key Site)(value London))
      ((key Date)(value 2025.08.08))
      ((key Round)(value 2))
      ((key White)(value Tom))
      ((key Black)(value Tim))
      ((key Result)(value *))
      ((key WhiteElo)(value 2000))
      ((key BlackElo)(value 2000))
      ((key TimeControl)(value"10 "))
      ((key Termination)(value *))
      ((key Link)(value https://www.chess.com/analysis))
      |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;
