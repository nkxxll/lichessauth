open Lwt.Infix
open Base

type tokenResponse =
  { token_type : string
  ; access_token : string
  ; expires_in : int
  }
[@@deriving show]

type user =
  { username : string
  ; _id : string
  ; _created_at : int
  }
[@@deriving show]

type opening =
  { eco : string
  ; name : string
  ; ply : int
  }
[@@deriving show, yojson]

type player =
  { name : string
  ; id : string
  }
[@@deriving show, yojson]

type side =
  | White
  | Black
[@@deriving show, yojson]

type arrow_color =
  | Red
  | Green
[@@deriving yojson]

type arrow =
  { start_square : string
  ; end_square : string
  ; color : arrow_color
  }
[@@deriving yojson]

type chessboard_options =
  { fen : string
  ; bad_move : arrow
  ; good_move : arrow
  ; orientation : side
  }
[@@deriving yojson]

type game =
  { id : string
  ; moves : string
  ; opening : opening (* white * black *)
  ; players : player * player
  ; winner : side
  }
[@@deriving show, yojson]

let redirect_url = "http://localhost:8080/redirect"
let email_url = "https://lichess.org/api/account/email"
let profile_url = "https://lichess.org/api/account"
let games_url user = "https://lichess.org/api/games/user/" ^ user
let request_url = "https://lichess.org/oauth"
let token_url = "https://lichess.org/api/token"
let client_id = "veryveryuniqueclientid"
let verifier = ref ""
let state = ref ""
let auth : tokenResponse ref = ref { access_token = ""; token_type = ""; expires_in = 0 }
let thisUser : user ref = ref { username = ""; _id = ""; _created_at = 0 }

let parse_user json =
  let open Yojson.Basic.Util in
  let username = json |> member "username" |> to_string in
  let id = json |> member "id" |> to_string in
  let created_at = json |> member "createdAt" |> to_int in
  { username; _id = id; _created_at = created_at }
;;

let games_to_json (games : game list) : Yojson.Safe.t =
  `List (List.map ~f:game_to_yojson games)
;;

let make_basic_get
      auth
      ?(headers = Cohttp.Header.init_with "Authorization" ("Bearer " ^ auth.access_token))
      url
  =
  let open Lwt.Infix in
  Cohttp_lwt_unix.Client.get ~headers url
  >>= fun (_res, body) -> Cohttp_lwt.Body.to_string body
;;

let parse_game (json : Yojson.Basic.t) : game =
  let open Yojson.Basic.Util in
  let id = json |> member "id" |> to_string in
  let moves = json |> member "moves" |> to_string in
  let opening_json = json |> member "opening" in
  let opening =
    { eco = opening_json |> member "eco" |> to_string
    ; name = opening_json |> member "name" |> to_string
    ; ply = opening_json |> member "ply" |> to_int
    }
  in
  let player1, player2 =
    let player_black_json = json |> member "players" |> member "black" |> member "user" in
    let player_white_json = json |> member "players" |> member "white" |> member "user" in
    let player1 =
      { name = player_white_json |> member "name" |> to_string
      ; id = player_white_json |> member "id" |> to_string
      }
    in
    let player2 =
      { name = player_black_json |> member "name" |> to_string
      ; id = player_black_json |> member "id" |> to_string
      }
    in
    player1, player2
  in
  let winner =
    let win_str = json |> member "winner" |> to_string in
    match win_str with
    | "black" -> Black
    | "white" -> White
    | _ -> failwith "winner is neither blach nor white"
  in
  { id; moves; opening; players = player1, player2; winner }
;;

let parse_game_list json_str =
  json_str
  |> String.split_lines
  (* filter last empty line *)
  |> List.filter ~f:(fun item -> not (String.equal item ""))
  |> List.map ~f:(fun item -> Yojson.Basic.from_string item |> parse_game)
;;

let rec fetch_follow_redirects ?(max_redirects = 5) uri ~headers ~body =
  if max_redirects = 0
  then Lwt.fail_with "Too many redirects"
  else
    Cohttp_lwt_unix.Client.post ~headers ~body uri
    >>= fun (resp, body) ->
    let status = Cohttp.Response.status resp in
    if Cohttp.Code.is_redirection (Cohttp.Code.code_of_status status)
    then (
      match Cohttp.Header.get (Cohttp.Response.headers resp) "location" with
      | Some location ->
        let new_uri = Uri.of_string location in
        fetch_follow_redirects ~max_redirects:(max_redirects - 1) new_uri ~headers ~body
      | None -> Lwt.fail_with "Redirection without Location header")
    else Lwt.return (resp, body)
;;

let create_challenge (code_verifier : string) : string =
  let module SHA256 = Digestif.SHA256 in
  let hash = SHA256.digest_string code_verifier |> SHA256.to_raw_string in
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet hash
;;

let random_string ~len =
  let random_str = Mirage_crypto_rng_unix.getrandom len in
  let encoded = Base64.encode_exn ~alphabet:Base64.uri_safe_alphabet random_str in
  encoded
;;

let e4nystyle () = Stdio.In_channel.read_all "./pgn/e4NYStyle_nl.pgn" |> String.strip
let e6b6nystyle () = Stdio.In_channel.read_all "./pgn/e6b6NYStyle_nl.pgn" |> String.strip

let () =
  Dream.run ~port:8080
  @@ Dream.logger
  @@ Dream.router
       [ Dream.get "/" (fun _ -> Dream.html "<a href=\"/login\">login</a>")
       ; Dream.get "/login" (fun req ->
           (* build the query params make the request to the request_url *)
           verifier := random_string ~len:96;
           state := random_string ~len:96;
           let challenge = create_challenge !verifier in
           let open Uri in
           let query =
             [ "response_type", [ "code" ]
             ; "client_id", [ client_id ]
             ; "redirect_uri", [ redirect_url ]
             ; "code_challenge_method", [ "S256" ]
             ; "code_challenge", [ challenge ]
             ; "scope", [ "email:read" ]
             ; "username", [ "" ]
             ; "state", [ !state ]
             ]
           in
           let full_url = with_query (Uri.of_string request_url) query in
           Dream.redirect req (Uri.to_string full_url))
       ; Dream.get "/redirect" (fun req ->
           match Dream.query req "state" with
           | None -> Dream.empty `Bad_Request
           | Some s ->
             if String.equal s !state
             then (
               match Dream.query req "code" with
               | None -> Dream.empty `Bad_Request
               | Some code ->
                 let form_data =
                   [ "grant_type", [ "authorization_code" ]
                   ; "code", [ code ]
                   ; "redirect_uri", [ redirect_url ]
                   ; "client_id", [ client_id ]
                   ; "code_verifier", [ !verifier ]
                   ]
                 in
                 let headers =
                   Cohttp.Header.init_with
                     "Content-Type"
                     "application/x-www-form-urlencoded"
                 in
                 let open Lwt.Infix in
                 fetch_follow_redirects
                   ~headers
                   ~body:(Cohttp_lwt.Body.of_string (Uri.encoded_of_query form_data))
                   (Uri.of_string token_url)
                 >>= fun (_, body) ->
                 Cohttp_lwt.Body.to_string body
                 >>= fun body_str ->
                 (* TODO this is a json yo decode that json *)
                 let json = Yojson.Basic.from_string body_str in
                 let open Yojson.Basic.Util in
                 let token_type = json |> member "token_type" |> to_string in
                 let access_token = json |> member "access_token" |> to_string in
                 let expires_in = json |> member "expires_in" |> to_int in
                 auth := { token_type; access_token; expires_in };
                 Dream.log
                   "token_type %s; expires_in %d"
                   !auth.token_type
                   !auth.expires_in;
                 make_basic_get !auth (Uri.of_string profile_url)
                 >>= fun body_str ->
                 let newUser = parse_user (Yojson.Basic.from_string body_str) in
                 thisUser := newUser;
                 Dream.log "This User:\n%s" (show_user !thisUser);
                 Dream.redirect req "http://localhost:5173/")
             else Dream.empty `Bad_Request)
       ; Dream.get "/email" (fun _req ->
           make_basic_get !auth (Uri.of_string email_url)
           >>= fun body_str -> Dream.respond ~status:`OK body_str)
       ; Dream.get "/gamelist" (fun _req ->
           let query =
             [ "max", [ "20" ]
             ; "opening", [ "true" ]
             ; "perfType", [ "blitz"; "rapid"; "classical" ]
             ]
           in
           let uri =
             Uri.with_query (Uri.of_string (games_url !thisUser.username)) query
           in
           let headers =
             Cohttp.Header.init_with "Authorization" ("Bearer " ^ !auth.access_token)
           in
           let headers = Cohttp.Header.add headers "Accept" "application/x-ndjson" in
           make_basic_get !auth uri ~headers
           >>= fun body ->
           (* dont wasted another thought on that but should probably *)
           let games = parse_game_list body in
           Dream.json (Yojson.Safe.to_string (games_to_json games)))
       ; Dream.get "/debug_games" (fun _req ->
           let query =
             [ "max", [ "20" ]
             ; "opening", [ "true" ]
             ; "perfType", [ "blitz"; "rapid"; "classical" ]
             ]
           in
           let uri =
             Uri.with_query (Uri.of_string (games_url !thisUser.username)) query
           in
           let headers =
             Cohttp.Header.init_with "Authorization" ("Bearer " ^ !auth.access_token)
           in
           let headers = Cohttp.Header.add headers "Accept" "application/x-ndjson" in
           make_basic_get !auth uri ~headers
           >>= fun body ->
           Dream.log "%s" body;
           let games = parse_game_list body in
           Dream.html (List.map ~f:show_game games |> String.concat ~sep:"\n"))
       ; Dream.get "/opening" (fun req ->
           match Dream.query req "color", Dream.query req "moves" with
           | Some color, Some moves ->
             (match
                Lichessserver.find_opening_error
                  ~white_file_str:(e4nystyle ())
                  ~black_file_str:(e6b6nystyle ())
                  moves
                  color
              with
              | Ok deviation -> Dream.json deviation
              | Error err -> Dream.html err ~status:`Internal_Server_Error)
           | _ -> Dream.empty `Bad_Request)
       ]
;;
