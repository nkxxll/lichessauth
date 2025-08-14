open Base
open Lichessserver

let e4nystyle = Stdio.In_channel.read_all "./pgn/e4NYStyle_nl.pgn" |> String.strip
let e6b6nystyle = Stdio.In_channel.read_all "./pgn/e6b6NYStyle_nl.pgn" |> String.strip

let () =
  (* Stdio.print_endline e6b6nystyle; *)
  (match parse_file_helper e4nystyle with
   | Ok _data -> Stdio.print_endline "e4 could be parsed"
   | Error err -> Stdio.print_endline err);
  match parse_file_helper e6b6nystyle with
  | Ok _data -> Stdio.print_endline "e6 could be parsed"
  | Error err -> Stdio.print_endline err
;;
