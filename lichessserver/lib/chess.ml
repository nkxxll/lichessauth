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

let key = take_while1 Char.is_alpha
let value = char '"' *> take_while1 (fun c -> not (Char.equal '"' c)) <* char '"'

let pair =
  char '[' *> key >>= fun key -> ws *> value <* char ']' >>| fun value -> { key; value }
;;

let config = sep_by (char '\n') pair

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
    [%expect {||}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;
