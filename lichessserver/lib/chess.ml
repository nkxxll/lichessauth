open Angstrom
open Base

(* lol this is not in 17 this is only in base 18 *)
let ( >> ) f g x = g (f x)

type config =
  { key : string
  ; value : string
  }
[@@deriving sexp]

type game_result =
  | White
  | Black
  | Draw
  | Other
[@@deriving sexp]

type token =
  | Number of int
  | Notation of string
  | LBracket
  | RBracket
  | Result of game_result
[@@deriving sexp]

let parse_file =
  satisfy (function
    | 'a' .. 'e' -> true
    | _ -> false)
;;

let parse_rank =
  satisfy (function
    | '1' .. '8' -> true
    | _ -> false)
  >>| fun r -> Char.to_int r - Char.to_int '0'
;;

type piece_type =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
[@@deriving show, compare, equal]

type move_info =
  { piece : piece_type
  ; from_file : char option
  ; from_rank : int option
  ; to_file : char
  ; to_rank : int
  ; promotion : piece_type option
  ; is_capture : bool
  ; is_check : bool
  ; is_checkmate : bool
  ; is_castle_kingside : bool
  ; is_castle_queenside : bool
  }
[@@deriving show]

let parse_piece_type =
  satisfy (function
    | 'K' | 'Q' | 'R' | 'B' | 'N' -> true
    | _ -> false)
  >>| function
  | 'K' -> King
  | 'Q' -> Queen
  | 'R' -> Rook
  | 'B' -> Bishop
  | 'N' -> Knight
  | _ -> failwith "unreachable"
;;

let parse_promotion = char '=' *> parse_piece_type >>| fun p -> Some p

let parse_check_or_mate =
  option
    (false, false)
    (satisfy (function
       | '+' | '#' -> true
       | _ -> false)
     >>| function
     | '+' -> true, false
     | '#' -> false, true
     | _ -> failwith "unreachable")
;;

(* yes to_file and to_rank have weird values but we filter this out before making a move *)
let parse_castle =
  string "O-O-O"
  >>| (fun _ ->
  { piece = King
  ; from_file = None
  ; from_rank = None
  ; to_file = ' '
  ; to_rank = -1
  ; promotion = None
  ; is_capture = false
  ; is_check = false
  ; is_checkmate = false
  ; is_castle_kingside = false
  ; is_castle_queenside = true
  })
  <|> (string "O-O"
       >>| fun _ ->
       { piece = King
       ; from_file = None
       ; from_rank = None
       ; to_file = ' '
       ; to_rank = -1
       ; promotion = None
       ; is_capture = false
       ; is_check = false
       ; is_checkmate = false
       ; is_castle_kingside = true
       ; is_castle_queenside = false
       })
;;

let parse_pawn_move =
  parse_file
  >>= fun from_file ->
  option false (char 'x' *> return true)
  >>= fun is_capture ->
  parse_file
  >>= fun to_file ->
  parse_rank
  >>= fun to_rank ->
  option None parse_promotion
  >>= fun promotion ->
  parse_check_or_mate
  >>= fun (is_check, is_checkmate) ->
  return
    { piece = Pawn
    ; from_file = Some from_file
    ; from_rank = None
    ; to_file = to_file
    ; to_rank = to_rank
    ; promotion
    ; is_capture
    ; is_check
    ; is_checkmate
    ; is_castle_kingside = false
    ; is_castle_queenside = false
    }
;;

let parse_piece_move =
  parse_piece_type
  >>= fun piece ->
  option None (parse_file >>| Option.some)
  >>= fun from_file ->
  option None (parse_rank >>| Option.some)
  >>= fun from_rank ->
  option false (char 'x' *> return true)
  >>= fun is_capture ->
  parse_file
  >>= fun to_file ->
  parse_rank
  >>= fun to_rank ->
  option None parse_promotion
  >>= fun promotion ->
  parse_check_or_mate
  >>= fun (is_check, is_checkmate) ->
  return
    { piece
    ; from_file
    ; from_rank
    ; to_file = to_file
    ; to_rank = to_rank
    ; promotion
    ; is_capture
    ; is_check
    ; is_checkmate
    ; is_castle_kingside = false
    ; is_castle_queenside = false
    }
;;

let parse_san =
  parse_castle
  <|> (peek_char
       >>= function
       | Some ('K' | 'Q' | 'R' | 'B' | 'N') -> parse_piece_move
       | _ -> parse_pawn_move)
;;

let parse_san_exn san =
    match parse_string ~consume:All parse_san san with
      | Ok mi -> mi
      | Error _ -> failwith "san could not be parsed!"

let ws =
  skip_many1
    (satisfy (function
       | ' ' | '\n' | '\t' | '\r' -> true
       | _ -> false))
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
  take_while1 Char.is_digit
  <* many1 (char '.')
  >>| fun number -> Number (Int.of_string number)
;;

let notation =
  take_while1 (function
    | c when Char.is_alphanum c -> true
    | '+' | '=' | '#' | '-' -> true
    | _ -> false)
  >>| fun not -> Notation not
;;

let l_bracket = char '(' >>| fun _ -> LBracket
let r_bracket = char ')' >>| fun _ -> RBracket
let white = string "1-0" >>| fun _ -> White
let black = string "0-1" >>| fun _ -> Black
let draw = string "1/2-1/2" >>| fun _ -> Draw
let other = string "*" >>| fun _ -> Other
let result = choice [ white; black; draw; other ] >>| fun r -> Result r
let config = sep_by (char '\n') config_pair

let move_section =
  many (many not_relevant *> choice [ result; number; notation; l_bracket; r_bracket ])
;;

let simple_move_list = sep_by (char ' ') notation
let game = config >>= fun c -> many not_relevant *> move_section >>| fun moves -> c, moves

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

let%expect_test "config one move" =
  let text = {|1. e4 e5|} in
  match parse_string ~consume:All move_section text with
  | Ok tokens ->
    tokens
    |> List.iter ~f:(fun c -> c |> sexp_of_token |> Sexp.to_string |> Stdio.print_endline);
    [%expect
      {|
      (Number 1)
      (Notation e4)
      (Notation e5)
      |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "config one move" =
  let text =
    {|1. e4 e5 (1... e6 2. d4 b6 3. Nf3 Bb7 4. Nc3 Nf6 (4... Bb4 5. Bd3 Nf6 6. Qe2 d5 7. e5 Ne4 8. Bxe4 (8. Bd2 Nxd2 9. Qxd2 Be7 10. Ne2) 8... dxe4 9. Nd2))|}
  in
  match parse_string ~consume:All move_section text with
  | Ok tokens ->
    tokens
    |> List.iter ~f:(fun c -> c |> sexp_of_token |> Sexp.to_string |> Stdio.print_endline);
    [%expect
      {|
      (Number 1)
      (Notation e4)
      (Notation e5)
      LBracket
      (Number 1)
      (Notation e6)
      (Number 2)
      (Notation d4)
      (Notation b6)
      (Number 3)
      (Notation Nf3)
      (Notation Bb7)
      (Number 4)
      (Notation Nc3)
      (Notation Nf6)
      LBracket
      (Number 4)
      (Notation Bb4)
      (Number 5)
      (Notation Bd3)
      (Notation Nf6)
      (Number 6)
      (Notation Qe2)
      (Notation d5)
      (Number 7)
      (Notation e5)
      (Notation Ne4)
      (Number 8)
      (Notation Bxe4)
      LBracket
      (Number 8)
      (Notation Bd2)
      (Notation Nxd2)
      (Number 9)
      (Notation Qxd2)
      (Notation Be7)
      (Number 10)
      (Notation Ne2)
      RBracket
      (Number 8)
      (Notation dxe4)
      (Number 9)
      (Notation Nd2)
      RBracket
      RBracket
      |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "comment" =
  let text = {|{some very cool +++ comment}|} in
  match parse_string ~consume:All not_relevant text with
  | Ok () ->
    Stdio.print_endline "unit";
    [%expect {|unit|}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "config one move" =
  let text =
    {| 1. e4 e5 (1... e6 2. d4 b6 3. Nf3 Bb7 4. Nc3 Nf6 (4... Bb4 5. Bd3 Nf6 6. Qe2 d5 7. e5 Ne4 8. Bxe4 (8. Bd2 Nxd2 9. Qxd2 Be7 {+0.41} 10. Ne2 {+0.48}) 8... dxe4 9. Nd2))|}
  in
  match parse_string ~consume:All move_section text with
  | Ok tokens ->
    tokens
    |> List.iter ~f:(fun c -> c |> sexp_of_token |> Sexp.to_string |> Stdio.print_endline);
    [%expect
      {|
      (Number 1)
      (Notation e4)
      (Notation e5)
      LBracket
      (Number 1)
      (Notation e6)
      (Number 2)
      (Notation d4)
      (Notation b6)
      (Number 3)
      (Notation Nf3)
      (Notation Bb7)
      (Number 4)
      (Notation Nc3)
      (Notation Nf6)
      LBracket
      (Number 4)
      (Notation Bb4)
      (Number 5)
      (Notation Bd3)
      (Notation Nf6)
      (Number 6)
      (Notation Qe2)
      (Notation d5)
      (Number 7)
      (Notation e5)
      (Notation Ne4)
      (Number 8)
      (Notation Bxe4)
      LBracket
      (Number 8)
      (Notation Bd2)
      (Notation Nxd2)
      (Number 9)
      (Notation Qxd2)
      (Notation Be7)
      (Number 10)
      (Notation Ne2)
      RBracket
      (Number 8)
      (Notation dxe4)
      (Number 9)
      (Notation Nd2)
      RBracket
      RBracket
      |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "moves for full games" =
  let text =
    {|1. e4 e5 (1... e6 2. d4 b6 3. Nf3 Bb7 4. Nc3 Nf6 (4... Bb4 5. Bd3 Nf6 6. Qe2 d5 7. e5 Ne4 8. Bxe4 (8. Bd2 Nxd2 9. Qxd2 Be7 {+0.41} 10. Ne2 {+0.48}) 8... dxe4 9. Nd2)) 2. f4 exf4 3. Nf3 g5 4. Bc4 (4. Nc3 g4 5. Ne5 Qh4+ 6. g3) *|}
  in
  let text2 =
    {|1. e4 e5 2. f4 d5 3. exd5 exf4 4. Nf3 (4. Nf3 c6 {-0.26} 5. Nc3 Bd6 {-0.19} 6.
d4 {-0.02} 6... Ne7 {-0.07} 7. Bd3 {-0.05} 7... cxd5 {-0.28} 8. O-O {-0.15} 8...
Nbc6 {-0.15} (8... h6 {-0.14} 9. Ne5 {-0.14} 9... Bxe5 {+0.20} 10. Bb5+ {-1.49}
10... Bd7 {-1.41} 11. dxe5 {-2.95} 11... Bxb5 {-1.05}) 9. Nb5 {-0.27} 9... Bg4
{-0.29} 10. Qe1 {-0.23} 10... Bxf3 {-0.11} 11. Rxf3 {-0.08} 11... Bb8 {-0.21})
(4. Nf3 Bd6 {-0.24} 5. c4 {-0.39} 5... b5 {+0.04} 6. b3 {0.00} 6... c6 {+0.16}
      7. Nc3 {+0.13} 7... Bf5 {+0.32}) *|}
  in
  (match parse_string ~consume:All move_section text with
   | Ok tokens ->
     tokens
     |> List.iter ~f:(fun c ->
       c |> sexp_of_token |> Sexp.to_string |> Stdio.print_endline);
     [%expect
       {|
       (Number 1)
       (Notation e4)
       (Notation e5)
       LBracket
       (Number 1)
       (Notation e6)
       (Number 2)
       (Notation d4)
       (Notation b6)
       (Number 3)
       (Notation Nf3)
       (Notation Bb7)
       (Number 4)
       (Notation Nc3)
       (Notation Nf6)
       LBracket
       (Number 4)
       (Notation Bb4)
       (Number 5)
       (Notation Bd3)
       (Notation Nf6)
       (Number 6)
       (Notation Qe2)
       (Notation d5)
       (Number 7)
       (Notation e5)
       (Notation Ne4)
       (Number 8)
       (Notation Bxe4)
       LBracket
       (Number 8)
       (Notation Bd2)
       (Notation Nxd2)
       (Number 9)
       (Notation Qxd2)
       (Notation Be7)
       (Number 10)
       (Notation Ne2)
       RBracket
       (Number 8)
       (Notation dxe4)
       (Number 9)
       (Notation Nd2)
       RBracket
       RBracket
       (Number 2)
       (Notation f4)
       (Notation exf4)
       (Number 3)
       (Notation Nf3)
       (Notation g5)
       (Number 4)
       (Notation Bc4)
       LBracket
       (Number 4)
       (Notation Nc3)
       (Notation g4)
       (Number 5)
       (Notation Ne5)
       (Notation Qh4+)
       (Number 6)
       (Notation g3)
       RBracket
       (Result Other)
       |}]
   | Error err ->
     Stdio.print_endline err;
     [%expect.unreachable]);
  match parse_string ~consume:All move_section text2 with
  | Ok tokens ->
    tokens
    |> List.iter ~f:(fun c -> c |> sexp_of_token |> Sexp.to_string |> Stdio.print_endline);
    [%expect
      {|
      (Number 1)
      (Notation e4)
      (Notation e5)
      (Number 2)
      (Notation f4)
      (Notation d5)
      (Number 3)
      (Notation exd5)
      (Notation exf4)
      (Number 4)
      (Notation Nf3)
      LBracket
      (Number 4)
      (Notation Nf3)
      (Notation c6)
      (Number 5)
      (Notation Nc3)
      (Notation Bd6)
      (Number 6)
      (Notation d4)
      (Number 6)
      (Notation Ne7)
      (Number 7)
      (Notation Bd3)
      (Number 7)
      (Notation cxd5)
      (Number 8)
      (Notation O-O)
      (Number 8)
      (Notation Nbc6)
      LBracket
      (Number 8)
      (Notation h6)
      (Number 9)
      (Notation Ne5)
      (Number 9)
      (Notation Bxe5)
      (Number 10)
      (Notation Bb5+)
      (Number 10)
      (Notation Bd7)
      (Number 11)
      (Notation dxe5)
      (Number 11)
      (Notation Bxb5)
      RBracket
      (Number 9)
      (Notation Nb5)
      (Number 9)
      (Notation Bg4)
      (Number 10)
      (Notation Qe1)
      (Number 10)
      (Notation Bxf3)
      (Number 11)
      (Notation Rxf3)
      (Number 11)
      (Notation Bb8)
      RBracket
      LBracket
      (Number 4)
      (Notation Nf3)
      (Notation Bd6)
      (Number 5)
      (Notation c4)
      (Number 5)
      (Notation b5)
      (Number 6)
      (Notation b3)
      (Number 6)
      (Notation c6)
      (Number 7)
      (Notation Nc3)
      (Number 7)
      (Notation Bf5)
      RBracket
      (Result Other)
      |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "parse game 3" =
  let text3 =
    {|[Event "?"]
[Site "?"]
[Date "????.??.??"]
[Round "?"]
[White "?"]
[Black "?"]
[Result "*"]
[Link "https://www.chess.com/analysis/game/pgn/26CErLFvbg/analysis"]

1. d4 e6 2. Bf4 b6 3. e3 g6 (3... Bb7 4. Nf3 Nf6 5. Bd3 Nh5 6. Bg5 Be7 7. Bxe7
Qxe7 {+0.22} 8. O-O O-O 9. c4 Nf6 10. Nc3 d5 11. cxd5 exd5 12. Rc1 c5 13. dxc5
{+0.26} 13... bxc5 14. Qa4 Nbd7 15. Rfd1 Nb6 16. Qh4 g6 17. Na4 {+0.37}) 4. Nf3
Bg7 (4... Bb7 5. Bd3 Nf6 6. Qe2 c5 7. Nc3 Nc6 8. O-O {+0.39} 8... Be7 9. Nb5 d6
10. c4 O-O 11. Rad1 {+0.44}) 5. Bd3 f5 6. c3 Nf6 7. Nbd2 Bb7 8. O-O (8. Qe2
{+0.40} 8... d6 {+0.44} 9. O-O-O {+0.42} 9... Nc6 {+0.54} 10. Rhe1 {+0.40} 10...
d5 {+0.52} 11. h4 {+0.59}) 8... d6 9. Re1 Nbd7 10. a4 Qe7 *|}
  in
  match parse_string ~consume:All game text3 with
  | Ok (config, tokens) ->
    config
    |> List.iter ~f:(fun c ->
      c |> sexp_of_config |> Sexp.to_string |> Stdio.print_endline);
    tokens
    |> List.iter ~f:(fun m -> m |> sexp_of_token |> Sexp.to_string |> Stdio.print_endline);
    [%expect
      {|
      ((key Event)(value ?))
      ((key Site)(value ?))
      ((key Date)(value ????.??.??))
      ((key Round)(value ?))
      ((key White)(value ?))
      ((key Black)(value ?))
      ((key Result)(value *))
      ((key Link)(value https://www.chess.com/analysis/game/pgn/26CErLFvbg/analysis))
      (Number 1)
      (Notation d4)
      (Notation e6)
      (Number 2)
      (Notation Bf4)
      (Notation b6)
      (Number 3)
      (Notation e3)
      (Notation g6)
      LBracket
      (Number 3)
      (Notation Bb7)
      (Number 4)
      (Notation Nf3)
      (Notation Nf6)
      (Number 5)
      (Notation Bd3)
      (Notation Nh5)
      (Number 6)
      (Notation Bg5)
      (Notation Be7)
      (Number 7)
      (Notation Bxe7)
      (Notation Qxe7)
      (Number 8)
      (Notation O-O)
      (Notation O-O)
      (Number 9)
      (Notation c4)
      (Notation Nf6)
      (Number 10)
      (Notation Nc3)
      (Notation d5)
      (Number 11)
      (Notation cxd5)
      (Notation exd5)
      (Number 12)
      (Notation Rc1)
      (Notation c5)
      (Number 13)
      (Notation dxc5)
      (Number 13)
      (Notation bxc5)
      (Number 14)
      (Notation Qa4)
      (Notation Nbd7)
      (Number 15)
      (Notation Rfd1)
      (Notation Nb6)
      (Number 16)
      (Notation Qh4)
      (Notation g6)
      (Number 17)
      (Notation Na4)
      RBracket
      (Number 4)
      (Notation Nf3)
      (Notation Bg7)
      LBracket
      (Number 4)
      (Notation Bb7)
      (Number 5)
      (Notation Bd3)
      (Notation Nf6)
      (Number 6)
      (Notation Qe2)
      (Notation c5)
      (Number 7)
      (Notation Nc3)
      (Notation Nc6)
      (Number 8)
      (Notation O-O)
      (Number 8)
      (Notation Be7)
      (Number 9)
      (Notation Nb5)
      (Notation d6)
      (Number 10)
      (Notation c4)
      (Notation O-O)
      (Number 11)
      (Notation Rad1)
      RBracket
      (Number 5)
      (Notation Bd3)
      (Notation f5)
      (Number 6)
      (Notation c3)
      (Notation Nf6)
      (Number 7)
      (Notation Nbd2)
      (Notation Bb7)
      (Number 8)
      (Notation O-O)
      LBracket
      (Number 8)
      (Notation Qe2)
      (Number 8)
      (Notation d6)
      (Number 9)
      (Notation O-O-O)
      (Number 9)
      (Notation Nc6)
      (Number 10)
      (Notation Rhe1)
      (Number 10)
      (Notation d5)
      (Number 11)
      (Notation h4)
      RBracket
      (Number 8)
      (Notation d6)
      (Number 9)
      (Notation Re1)
      (Notation Nbd7)
      (Number 10)
      (Notation a4)
      (Notation Qe7)
      (Result Other)
      |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

module ChessNode = struct
  open Sexplib0

  type t =
    { notation : string
    ; mutable next : t list
    ; mutable parent : t option
    }

  (* Custom S-expression converter *)
  let rec sexp_of_t (n : t) : Sexp.t =
    Sexp.List [ Sexp.Atom n.notation; Sexp.List (List.map ~f:sexp_of_t n.next) ]
  ;;

  let rec t_of_sexp (sexp : Sexp.t) : t =
    match sexp with
    | Sexp.List [ Sexp.Atom notation; Sexp.List children ] ->
      { notation; next = List.map ~f:t_of_sexp children; parent = None }
    | _ -> failwith "Invalid node sexp"
  ;;

  (* Utility to reconnect parent links after parsing *)
  let rec set_parents parent node =
    node.parent <- parent;
    List.iter ~f:(set_parents (Some node)) node.next
  ;;
end

(* this is not a effect free function we are changing the next list of the base_node tree *)
let merge_nodes base_node new_node =
  let open ChessNode in
  (* Merge children from new_node into base_node *)
  let rec loop base_node new_node =
    List.iter
      ~f:(fun new_child ->
        match
          List.find
            ~f:(fun existing -> String.equal existing.notation new_child.notation)
            base_node.next
        with
        | Some existing_child ->
          (* Merge recursively if move already exists *)
          loop existing_child new_child
        | None ->
          (* Add as new variation *)
          base_node.next <- base_node.next @ [ new_child ])
      new_node.next
  in
  loop base_node new_node;
  base_node
;;

(* Parse tokens into a node tree with alternatives *)
let parse_tokens tokens =
  let tokens =
    List.filter tokens ~f:(function
      | Result _ | Number _ -> false
      | _ -> true)
  in
  let root = { ChessNode.notation = "root"; next = []; parent = None } in
  (* parse a sequence of moves until the end or a RBracket *)
  (* we need a last in here to add the variation if we find them *)
  (* last -> current -> LBracket -> var to current which is added to last.next *)
  let rec parse_sequence last toks =
    match toks with
    | [] -> []
    | RBracket :: tl -> tl
    | Notation no :: tl ->
      let new_node = { ChessNode.notation = no; next = []; parent = Some last } in
      last.next <- new_node :: last.next;
      parse_sequence new_node tl
    | LBracket :: tl ->
      let rest =
        match last.parent with
        | Some p -> parse_sequence p tl
        | None -> failwith "should have a parent"
      in
      parse_sequence last rest
    | _ -> failwith "there are no other tokens"
  in
  let _rest = parse_sequence root tokens in
  root
;;

let rec print_tree node =
  let open ChessNode in
  Stdio.print_endline ("Node: " ^ node.notation);
  Stdio.printf "Next:";
  List.iter node.next ~f:(fun n -> Stdio.printf " %s" n.notation);
  Stdio.print_endline "";
  List.iter node.next ~f:print_tree
;;

let%expect_test "tree from tokens" =
  let game_str =
    {|[Event "?"]
[Site "?"]
[Date "????.??.??"]
[Round "?"]
[White "?"]
[Black "?"]
[Result "*"]
[Link "https://www.chess.com/analysis/game/pgn/26CErLFvbg/analysis"]

1. e4 e5 (1... e6) 2. f4 (2. Nc3 Nc6) d5 *|}
  in
  match parse_string ~consume:All game game_str with
  | Ok (_config, tokens) ->
    let parsed = parse_tokens tokens in
    print_tree parsed;
    [%expect
      {|
      Node: root
      Next: e4
      Node: e4
      Next: e6 e5
      Node: e6
      Next:
      Node: e5
      Next: Nc3 f4
      Node: Nc3
      Next: Nc6
      Node: Nc6
      Next:
      Node: f4
      Next: d5
      Node: d5
      Next:
      |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "tree from tokens" =
  let game_str =
    {|[Event "?"]
[Site "?"]
[Date "????.??.??"]
[Round "?"]
[White "?"]
[Black "?"]
[Result "*"]
[Link "https://www.chess.com/analysis/game/pgn/26CErLFvbg/analysis"]

1. d4 e6 2. Bf4 b6 3. e3 g6 (3... Bb7 4. Nf3 Nf6 5. Bd3 Nh5 6. Bg5 Be7 7. Bxe7
Qxe7 {+0.22} 8. O-O O-O 9. c4 Nf6 10. Nc3 d5 11. cxd5 exd5 12. Rc1 c5 13. dxc5
{+0.26} 13... bxc5 14. Qa4 Nbd7 15. Rfd1 Nb6 16. Qh4 g6 17. Na4 {+0.37}) 4. Nf3
Bg7 (4... Bb7 5. Bd3 Nf6 6. Qe2 c5 7. Nc3 Nc6 8. O-O {+0.39} 8... Be7 9. Nb5 d6
10. c4 O-O 11. Rad1 {+0.44}) 5. Bd3 f5 6. c3 Nf6 7. Nbd2 Bb7 8. O-O (8. Qe2
{+0.40} 8... d6 {+0.44} 9. O-O-O {+0.42} 9... Nc6 {+0.54} 10. Rhe1 {+0.40} 10...
d5 {+0.52} 11. h4 {+0.59}) 8... d6 9. Re1 Nbd7 10. a4 Qe7 *|}
  in
  match parse_string ~consume:All game game_str with
  | Ok (_config, tokens) ->
    let parsed = parse_tokens tokens in
    print_tree parsed;
    [%expect
      {|
      Node: root
      Next: d4
      Node: d4
      Next: e6
      Node: e6
      Next: Bf4
      Node: Bf4
      Next: b6
      Node: b6
      Next: e3
      Node: e3
      Next: Bb7 g6
      Node: Bb7
      Next: Nf3
      Node: Nf3
      Next: Nf6
      Node: Nf6
      Next: Bd3
      Node: Bd3
      Next: Nh5
      Node: Nh5
      Next: Bg5
      Node: Bg5
      Next: Be7
      Node: Be7
      Next: Bxe7
      Node: Bxe7
      Next: Qxe7
      Node: Qxe7
      Next: O-O
      Node: O-O
      Next: O-O
      Node: O-O
      Next: c4
      Node: c4
      Next: Nf6
      Node: Nf6
      Next: Nc3
      Node: Nc3
      Next: d5
      Node: d5
      Next: cxd5
      Node: cxd5
      Next: exd5
      Node: exd5
      Next: Rc1
      Node: Rc1
      Next: c5
      Node: c5
      Next: dxc5
      Node: dxc5
      Next: bxc5
      Node: bxc5
      Next: Qa4
      Node: Qa4
      Next: Nbd7
      Node: Nbd7
      Next: Rfd1
      Node: Rfd1
      Next: Nb6
      Node: Nb6
      Next: Qh4
      Node: Qh4
      Next: g6
      Node: g6
      Next: Na4
      Node: Na4
      Next:
      Node: g6
      Next: Nf3
      Node: Nf3
      Next: Bb7 Bg7
      Node: Bb7
      Next: Bd3
      Node: Bd3
      Next: Nf6
      Node: Nf6
      Next: Qe2
      Node: Qe2
      Next: c5
      Node: c5
      Next: Nc3
      Node: Nc3
      Next: Nc6
      Node: Nc6
      Next: O-O
      Node: O-O
      Next: Be7
      Node: Be7
      Next: Nb5
      Node: Nb5
      Next: d6
      Node: d6
      Next: c4
      Node: c4
      Next: O-O
      Node: O-O
      Next: Rad1
      Node: Rad1
      Next:
      Node: Bg7
      Next: Bd3
      Node: Bd3
      Next: f5
      Node: f5
      Next: c3
      Node: c3
      Next: Nf6
      Node: Nf6
      Next: Nbd2
      Node: Nbd2
      Next: Bb7
      Node: Bb7
      Next: Qe2 O-O
      Node: Qe2
      Next: d6
      Node: d6
      Next: O-O-O
      Node: O-O-O
      Next: Nc6
      Node: Nc6
      Next: Rhe1
      Node: Rhe1
      Next: d5
      Node: d5
      Next: h4
      Node: h4
      Next:
      Node: O-O
      Next: d6
      Node: d6
      Next: Re1
      Node: Re1
      Next: Nbd7
      Node: Nbd7
      Next: a4
      Node: a4
      Next: Qe7
      Node: Qe7
      Next:
      |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "merge two simple move trees" =
  let game1 =
    {|[Event "Example 1"]
[Site "?"]
[Date "2025.08.12"]
[Round "-"]
[White "White"]
[Black "Black"]
[Result "*"]

1. e4 e5 2. Nc3 Nf6 3. f4 d5 4. fxe5 Nxe4 *|}
  in
  let game2 =
    {|[Event "Example 2"]
[Site "?"]
[Date "2025.08.12"]
[Round "-"]
[White "White"]
[Black "Black"]
[Result "*"]

1. e4 e5 2. Nc3 Nf6 3. f4 d5 4. exd5 Nxd5 *|}
  in
  let quick_parse game_str =
    match parse_string ~consume:All game game_str with
    | Ok (_config, toks) -> toks
    | Error _ -> failwith "cannot happen in this test"
  in
  let base = quick_parse game1 in
  let new_game = quick_parse game2 in
  let base = merge_nodes (parse_tokens base) (parse_tokens new_game) in
  print_tree base;
  [%expect
    {|
    Node: root
    Next: e4
    Node: e4
    Next: e5
    Node: e5
    Next: Nc3
    Node: Nc3
    Next: Nf6
    Node: Nf6
    Next: f4
    Node: f4
    Next: d5
    Node: d5
    Next: fxe5 exd5
    Node: fxe5
    Next: Nxe4
    Node: Nxe4
    Next:
    Node: exd5
    Next: Nxd5
    Node: Nxd5
    Next:
    |}]
;;

let%expect_test "san notation parsing" =
  let sans =
    [ "e4"
    ; "exd5"
    ; "exd6"
    ; "e8=Q"
    ; "exd8=R"
    ; "e8=Q+"
    ; "e8=Q#"
    ; "exd8=Q#"
    ; "Nf3"
    ; "Qh5"
    ; "Bxc4"
    ; "Nf7+"
    ; "Qh7#"
    ; "Rxe8#"
    ; "Nbd7"
    ; "R1e4"
    ; "Qh4e1"
    ; "O-O"
    ; "O-O-O"
    ; "O-O+"
    ; "O-O-O#"
    ; "Raxd8+"
    ; "Nfxe8=Q"
    ; "fxg8=R#"
    ]
  in
  let parse_helper san =
    match parse_string ~consume:All parse_san san with
    | Ok s -> s
    | Error _ -> failwith "cannot happen"
  in
  List.iter sans ~f:(parse_helper >> show_move_info >> Stdio.print_endline);
  [%expect {||}]
;;
