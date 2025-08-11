open Angstrom
open Base

type config =
  { key : string
  ; value : string
  }
[@@deriving show]

type move =
  { number : int
  ; white : string
  ; black : string
  }
[@@deriving show]

type move_node =
  | Move of move
  | Deviation of move list
[@@deriving show]

type move_tree = move_node list [@@deriving show]

type result =
  | Anything
  | WhiteWon
  | BlackWon
  | Draw
[@@deriving show]

type pgn =
  { config : config list
  ; moves : move_tree
  }
[@@deriving show]

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

let number = take_while1 Char.is_digit <* char '.' >>| Int.of_string
let move = take_while1 Char.is_alphanum

let full_move =
  number
  >>= fun num ->
  not_relevant *> move
  >>= fun m1 ->
  option "" (not_relevant *> move) >>| fun m2 -> { number = num; white = m1; black = m2 }
;;

let white_won = string "1-0" >>| fun _ -> WhiteWon
let black_won = string "0-1" >>| fun _ -> BlackWon
let draw = string "1/2-1/2" >>| fun _ -> Draw
let anything = string "*" >>| fun _ -> Anything

(* NOTE: this can be way faster with a peek_char and then return the res but this is way easier *)
let result = choice [ white_won; black_won; draw; anything ]

let moves =
  sep_by not_relevant full_move >>= fun ms -> not_relevant *> result >>| fun r -> ms, r
;;

let config = sep_by (char '\n') config_pair
let game = config >>= fun c -> not_relevant *> moves >>| fun ms -> c, ms

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
    res |> List.iter ~f:(fun c -> c |> show_config |> Stdio.print_endline);
    [%expect {|
      { Chess.key = "Event"; value = "Test" }
      { Chess.key = "Site"; value = "London" }
      { Chess.key = "Date"; value = "2025.08.08" }
      { Chess.key = "Round"; value = "2" }
      { Chess.key = "White"; value = "Tom" }
      { Chess.key = "Black"; value = "Tim" }
      { Chess.key = "Result"; value = "*" }
      { Chess.key = "WhiteElo"; value = "2000" }
      { Chess.key = "BlackElo"; value = "2000" }
      { Chess.key = "TimeControl"; value = "10 " }
      { Chess.key = "Termination"; value = "*" }
      { Chess.key = "Link"; value = "https://www.chess.com/analysis" }
      |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "some simple moves" =
  let text = {|1.e4 e5 2.f4 exf4 3.Nf3 g5 *|} in
  match parse_string ~consume:All moves text with
  | Ok res ->
    let ms, r = res in
    ms |> List.iter ~f:(fun r -> r |> show_move |> Stdio.print_endline);
    r |> show_result |> Stdio.print_endline;
    [%expect {|
      { Chess.number = 1; white = "e4"; black = "e5" }
      { Chess.number = 2; white = "f4"; black = "exf4" }
      { Chess.number = 3; white = "Nf3"; black = "g5" }
      Chess.Anything
      |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "simple game" =
  let text =
    {|[Event "?"]
[Site "?"]
[Date "????.??.??"]
[Round "?"]
[White "?"]
[Black "?"]
[Result "*"]
[Link "https://www.chess.com/analysis"]

1. e4 e5 2. f4 exf4 3. Nf3 g5 4. h4 g4 5. Bc4 gxf3 6. Qxf3 Nc6 7. d4 *|}
  in
  match parse_string ~consume:All game text with
  | Ok res ->
    let config, moves_res = res in
    config |> List.iter ~f:(fun c -> c |> show_config |> Stdio.print_endline);
    let moves, res = moves_res in
    moves |> List.iter ~f:(fun m -> m |> show_move |> Stdio.print_endline);
    res |> show_result |> Stdio.print_endline;
    [%expect {|
      { Chess.key = "Event"; value = "?" }
      { Chess.key = "Site"; value = "?" }
      { Chess.key = "Date"; value = "????.??.??" }
      { Chess.key = "Round"; value = "?" }
      { Chess.key = "White"; value = "?" }
      { Chess.key = "Black"; value = "?" }
      { Chess.key = "Result"; value = "*" }
      { Chess.key = "Link"; value = "https://www.chess.com/analysis" }
      { Chess.number = 1; white = "e4"; black = "e5" }
      { Chess.number = 2; white = "f4"; black = "exf4" }
      { Chess.number = 3; white = "Nf3"; black = "g5" }
      { Chess.number = 4; white = "h4"; black = "g4" }
      { Chess.number = 5; white = "Bc4"; black = "gxf3" }
      { Chess.number = 6; white = "Qxf3"; black = "Nc6" }
      { Chess.number = 7; white = "d4"; black = "" }
      Chess.Anything
      |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;
