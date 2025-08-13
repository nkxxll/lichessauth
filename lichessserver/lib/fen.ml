open Base

type figure_color =
  | White
  | Black
[@@deriving show, equal]

type figure = figure_color * Chess.piece_type [@@deriving show]

type square =
  | Empty
  | Figure of figure
[@@deriving show]

type pos =
  { row : int
  ; col : int
  }
[@@deriving show]

type move =
  { from_square : pos
  ; to_square : pos
  }
[@@deriving show]

let create_row figure = List.init 8 ~f:(fun _ -> figure)

(*
   R K B Q E B K R
P P P P P P P P
_ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _
P P P P P P P P
R K B Q E B K R
*)

let create_base_row color =
  [ Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook ]
  |> List.map ~f:(fun f -> Figure (color, f))
;;

let create_empty : square list list =
  let board = [ create_base_row White ] in
  let board = board @ [ create_row (Figure (White, Pawn)) ] in
  let board = board @ [ create_row Empty ] in
  let board = board @ [ create_row Empty ] in
  let board = board @ [ create_row Empty ] in
  let board = board @ [ create_row Empty ] in
  let board = board @ [ create_row (Figure (Black, Pawn)) ] in
  let board = board @ [ create_base_row Black ] in
  board
;;

let print_board board =
  List.iter (List.rev board) ~f:(fun l ->
    l
    |> List.iter ~f:(fun i ->
      (match i with
       | Empty -> '_'
       | Figure (c, Pawn) -> if equal_figure_color c White then 'P' else 'p'
       | Figure (c, King) -> if equal_figure_color c White then 'K' else 'k'
       | Figure (c, Queen) -> if equal_figure_color c White then 'Q' else 'q'
       | Figure (c, Knight) -> if equal_figure_color c White then 'N' else 'n'
       | Figure (c, Bishop) -> if equal_figure_color c White then 'B' else 'b'
       | Figure (c, Rook) -> if equal_figure_color c White then 'R' else 'r')
      |> Stdio.printf "%c ");
    Stdio.print_endline "")
;;

let make_move board move =
  let piece, board =
    ( (let row = List.nth_exn board move.from_square.row in
       List.nth_exn row move.from_square.col)
    , List.mapi board ~f:(fun i row ->
        if Int.equal move.from_square.row i
        then
          List.mapi row ~f:(fun j item ->
            if Int.equal move.from_square.col j then Empty else item)
        else row) )
  in
  List.mapi board ~f:(fun i row ->
    if Int.equal move.to_square.row i
    then
      List.mapi row ~f:(fun j item ->
        if Int.equal move.to_square.col j then piece else item)
    else row)
;;

let piece_to_char = function
  | Chess.Pawn -> 'p'
  | Chess.Knight -> 'n'
  | Chess.Rook -> 'r'
  | Chess.Bishop -> 'b'
  | Chess.Queen -> 'q'
  | Chess.King -> 'k'
;;

let square_to_char = function
  | Empty -> None
  | Figure (color, fig_type) ->
    let ch = piece_to_char fig_type in
    Some
      (match color with
       | White -> Char.uppercase ch
       | Black -> ch)
;;

let fen_of_board (board : square list list) : string =
  let rank_to_fen rank =
    let rec loop acc empties = function
      | [] ->
        let acc = if empties > 0 then Int.to_string empties :: acc else acc in
        List.rev acc |> String.concat ~sep:""
      | sq :: rest ->
        (match square_to_char sq with
         | None -> loop acc (empties + 1) rest
         | Some ch ->
           let acc = if empties > 0 then Int.to_string empties :: acc else acc in
           loop (String.make 1 ch :: acc) 0 rest)
    in
    loop [] 0 rank
  in
  board |> List.map ~f:rank_to_fen |> List.rev |> String.concat ~sep:"/"
;;

let is_empty board { row; col } =
  match List.nth_exn (List.nth_exn board row) col with
  | Empty -> true
  | _ -> false
;;

let has_no_blockers board from_pos to_pos =
  let dr = compare to_pos.row from_pos.row in
  let dc = compare to_pos.col from_pos.col in
  let rec loop r c =
    let r' = r + dr in
    let c' = c + dc in
    if r' = to_pos.row && c' = to_pos.col
    then true
    else if not (is_empty board { row = r'; col = c' })
    then false
    else loop r' c'
  in
  loop from_pos.row from_pos.col
;;

let is_legal_move board from_pos to_pos =
  let dr = to_pos.row - from_pos.row in
  let dc = to_pos.col - from_pos.col in
  match List.nth_exn (List.nth_exn board from_pos.row) from_pos.col with
  | Empty -> false
  | Figure (color, piece) ->
    (match piece with
     | Pawn ->
       let dir = if equal_figure_color color White then -1 else 1 in
       let start_row = if equal_figure_color color White then 6 else 1 in
       (* Move forward *)
       if dc = 0
       then
         if dr = dir && is_empty board to_pos
         then true
         else if
           from_pos.row = start_row
           && dr = 2 * dir
           && is_empty board to_pos
           && is_empty board { row = from_pos.row + dir; col = from_pos.col }
         then true
         else false (* Capture diagonally *)
       else if abs dc = 1 && dr = dir
       then (
         match List.nth_exn (List.nth_exn board to_pos.row) to_pos.col with
         | Figure (c, _) when not (equal_figure_color c color) -> true
         | _ -> false)
       else false
     | Knight -> (abs dr = 2 && abs dc = 1) || (abs dr = 1 && abs dc = 2)
     | Bishop -> abs dr = abs dc && has_no_blockers board from_pos to_pos
     | Rook -> (dr = 0 || dc = 0) && has_no_blockers board from_pos to_pos
     | Queen ->
       (abs dr = abs dc || dr = 0 || dc = 0) && has_no_blockers board from_pos to_pos
     | King -> abs dr <= 1 && abs dc <= 1)
;;

(* I hate chess all not zero indexed but my list of my board is *)
let file_to_int file = Char.to_int file - Char.to_int 'a' + 1

let move_from_pgn board color notation =
  let move_info = Chess.parse_san_exn notation in
  let dest_pos = { row = move_info.to_rank; col = file_to_int move_info.to_file } in
  (* Helper: does a piece match the SAN constraints *)
  let piece_matches pos square =
    match square with
    | Figure (c, t) ->
      equal_figure_color c color
      && Chess.equal_piece_type move_info.piece t
      && (match move_info.from_file with
          | Some f -> file_to_int f = pos.row
          | None -> true)
      &&
        (match move_info.from_rank with
        | Some r -> r = pos.row
        | None -> true)
    | Empty -> false
  in
  (* Find source position by scanning board *)
  let rec find_source r c =
    if r >= List.length board
    then failwith "No matching source square found"
    else if c >= List.length (List.nth_exn board r)
    then find_source (r + 1) 0
    else (
      let pos = { row = r; col = c } in
      let sq = List.nth_exn (List.nth_exn board r) c in
      if piece_matches pos sq
      then
        (* Extra check: can this piece legally move to dest_pos? *)
        (* This assumes you have some function is_legal_move *)
        if is_legal_move board pos dest_pos then pos else find_source r (c + 1)
      else find_source r (c + 1))
  in
  let source_pos = find_source 0 0 in
  make_move board { from_square = source_pos; to_square = dest_pos }
;;

let%expect_test "test board creation" =
  let board = create_empty in
  print_board board;
  [%expect
    {|
    r n b q k b n r
    p p p p p p p p
    _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _
    P P P P P P P P
    R N B Q K B N R
    |}]
;;

let%expect_test "test move on board" =
  let board = create_empty in
  let example_move : move =
    { from_square = { row = 1; col = 4 }
    ; (* e2 in chess coordinates if row 0 is rank 1 *)
      to_square = { row = 3; col = 4 } (* e4 *)
    }
  in
  make_move board example_move |> print_board;
  [%expect
    {|
    r n b q k b n r
    p p p p p p p p
    _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _
    _ _ _ _ P _ _ _
    _ _ _ _ _ _ _ _
    P P P P _ P P P
    R N B Q K B N R
    |}]
;;

let%expect_test "test move on board" =
  let board = create_empty in
  let moves : move list =
    [ (* 1. e4 e6 *)
      { from_square = { row = 1; col = 4 }; to_square = { row = 3; col = 4 } }
    ; (* e4 *)
      { from_square = { row = 6; col = 4 }; to_square = { row = 5; col = 4 } }
    ; (* 2. d4 b6 *)
      { from_square = { row = 1; col = 3 }; to_square = { row = 3; col = 3 } }
    ; (* d4 *)
      { from_square = { row = 6; col = 1 }; to_square = { row = 5; col = 1 } }
    ; (* b6 *)

      (* 3. Nc3 Bb7 *)
      { from_square = { row = 0; col = 1 }; to_square = { row = 2; col = 2 } }
    ; (* Nc3 *)
      { from_square = { row = 7; col = 2 }; to_square = { row = 6; col = 1 } }
    ; (* Bb7 *)

      (* 4. Nf3 Nf6 *)
      { from_square = { row = 0; col = 6 }; to_square = { row = 2; col = 5 } }
    ; (* Nf3 *)
      { from_square = { row = 7; col = 6 }; to_square = { row = 5; col = 5 } } (* Nf6 *)
    ]
  in
  List.fold moves ~init:board ~f:(fun board move -> make_move board move) |> print_board;
  [%expect
    {|
    r n _ q k b _ r
    p b p p _ p p p
    _ p _ _ p n _ _
    _ _ _ _ _ _ _ _
    _ _ _ P P _ _ _
    _ _ N _ _ N _ _
    P P P _ _ P P P
    R _ B Q K B _ R
    |}]
;;

let%expect_test "fen_of_board starting position" =
  let starting_board = create_empty in
  Stdio.print_endline (fen_of_board starting_board);
  [%expect {| rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR |}]
;;

let%expect_test "fen_of_board simple e4 pos" =
  let starting_board = create_empty in
  let moves : move list =
    [ (* 1. e4 e6 *)
      { from_square = { row = 1; col = 4 }; to_square = { row = 3; col = 4 } }
    ; (* e4 *)
      { from_square = { row = 6; col = 4 }; to_square = { row = 5; col = 4 } }
    ; (* 2. d4 b6 *)
      { from_square = { row = 1; col = 3 }; to_square = { row = 3; col = 3 } }
    ; (* d4 *)
      { from_square = { row = 6; col = 1 }; to_square = { row = 5; col = 1 } }
    ; (* b6 *)

      (* 3. Nc3 Bb7 *)
      { from_square = { row = 0; col = 1 }; to_square = { row = 2; col = 2 } }
    ; (* Nc3 *)
      { from_square = { row = 7; col = 2 }; to_square = { row = 6; col = 1 } }
    ; (* Bb7 *)

      (* 4. Nf3 Nf6 *)
      { from_square = { row = 0; col = 6 }; to_square = { row = 2; col = 5 } }
    ; (* Nf3 *)
      { from_square = { row = 7; col = 6 }; to_square = { row = 5; col = 5 } } (* Nf6 *)
    ]
  in
  List.fold moves ~init:starting_board ~f:(fun board move -> make_move board move)
  |> fen_of_board
  |> Stdio.print_endline;
  [%expect {| rn1qkb1r/pbpp1ppp/1p2pn2/8/3PP3/2N2N2/PPP2PPP/R1BQKB1R |}]
;;
