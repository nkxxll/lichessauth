open Angstrom
open Base

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

type node =
  { notation : string
  ; mutable next : node list
  }
[@@deriving sexp]

(* Parse tokens into a node tree with alternatives *)
let parse_tokens tokens =
  let tokens =
    List.filter tokens ~f:(function
      | Result _ | Number _ -> false
      | _ -> true)
  in
  (* parse a sequence of moves until the end or a RBracket *)
  let rec parse_sequence toks =
    match toks with
    | [] -> [], None
    | RBracket :: rest -> rest, None (* stop on right bracket *)
    | Notation m :: rest ->
      let current_node = { notation = m; next = [] } in
      let rest, next_node_opt = parse_sequence rest in
      (match next_node_opt with
       | None -> rest, Some current_node
       | Some next_node ->
         current_node.next <- [ next_node ];
         rest, Some current_node)
    | LBracket :: rest ->
      let rest, alts = parse_alternatives rest in
      let rest, next_node_opt = parse_sequence rest in
      let alt_node =
        match next_node_opt with
        | None -> { notation = ""; next = alts }
        | Some next_node -> { notation = ""; next = alts @ [ next_node ] }
      in
      rest, Some alt_node
    | _ :: rest -> parse_sequence rest (* skip unexpected tokens *)
  (* parse alternatives inside brackets *)
  and parse_alternatives toks =
    let rec loop acc toks =
      match toks with
      | [] -> [], List.rev acc (* no closing bracket, return what we have *)
      | RBracket :: rest -> rest, List.rev acc (* end of alternatives *)
      | _ ->
        let rest, seq_opt = parse_sequence toks in
        (match seq_opt with
         | None -> rest, List.rev acc
         | Some seq -> loop (seq :: acc) rest)
    in
    loop [] toks
  in
  let _, root_opt = parse_sequence tokens in
  match root_opt with
  | None -> { notation = ""; next = [] } (* empty tree *)
  | Some root -> root
;;

let rec print_tree node =
  sexp_of_node node |> Sexp.to_string |> Stdio.print_endline;
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

1. e4 e5 (1... e6) 2. f4 (2. Nc3 Nc6) e5 *|}
  in
  match parse_string ~consume:All game game_str with
  | Ok (_config, tokens) ->
    let parsed = parse_tokens tokens in
    sexp_of_node parsed |> Sexp.to_string_hum |> Stdio.print_endline;
    [%expect {||}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;
