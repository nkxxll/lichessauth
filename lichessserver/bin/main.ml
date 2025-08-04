open Lwt.Infix

let redirect_url = "http://localhost:8080/redirect"
let email_url = "https://lichess.org/api/account/email"
let request_url = "https://lichess.org/oauth"
let token_url = "https://lichess.org/api/token"
let client_id = "veryveryuniqueclientid"
let verifier = ref ""
let state = ref ""
let auth = ref ""

let rec fetch_follow_redirects ?(max_redirects = 5) uri ~headers ~body =
  if max_redirects = 0 then Lwt.fail_with "Too many redirects"
  else
    Cohttp_lwt_unix.Client.post ~headers ~body uri >>= fun (resp, body) ->
    let status = Cohttp.Response.status resp in
    if Cohttp.Code.is_redirection (Cohttp.Code.code_of_status status) then
      match Cohttp.Header.get (Cohttp.Response.headers resp) "location" with
      | Some location ->
          let new_uri = Uri.of_string location in
          fetch_follow_redirects ~max_redirects:(max_redirects - 1) new_uri
            ~headers ~body
      | None -> Lwt.fail_with "Redirection without Location header"
    else Lwt.return (resp, body)

let create_challenge (code_verifier : string) : string =
  let module SHA256 = Digestif.SHA256 in
  let hash = SHA256.digest_string code_verifier |> SHA256.to_raw_string in
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet hash

let random_string ~len =
  let random_str = Mirage_crypto_rng_unix.getrandom len in
  let encoded =
    Base64.encode_exn ~alphabet:Base64.uri_safe_alphabet random_str
  in
  encoded

let () =
  Dream.run ~port:8080 @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> Dream.html "<a href=\"/login\">login</a>");
         Dream.get "/login" (fun req ->
             (* build the query params make the request to the request_url *)
             verifier := random_string ~len:96;
             state := random_string ~len:96;
             let challenge = create_challenge !verifier in
             let open Uri in
             let query =
               [
                 ("response_type", [ "code" ]);
                 ("client_id", [ client_id ]);
                 ("redirect_uri", [ redirect_url ]);
                 ("code_challenge_method", [ "S256" ]);
                 ("code_challenge", [ challenge ]);
                 ("scope", [ "email:read" ]);
                 ("username", [ "" ]);
                 ("state", [ !state ]);
               ]
             in
             let full_url = with_query (Uri.of_string request_url) query in
             Dream.redirect req (Uri.to_string full_url));
         Dream.get "/redirect" (fun req ->
             match Dream.query req "state" with
             | None -> Dream.empty `Bad_Request
             | Some s ->
                 if String.equal s !state then (
                   match Dream.query req "code" with
                   | None -> Dream.empty `Bad_Request
                   | Some code ->
                       (* form.Set("client_id", CLIENT_ID) *)
                       (* form.Set("grant_type", "authorization_code") *)
                       (* form.Set("code", code) *)
                       (* form.Set("redirect_uri", "http://localhost:8080/redirect") *)
                       (* form.Set("code_verifier", codeVerification) *)
                       let form_data =
                         [
                           ("grant_type", [ "authorization_code" ]);
                           ("code", [ code ]);
                           ("redirect_uri", [ redirect_url ]);
                           ("client_id", [ client_id ]);
                           ("code_verifier", [ !verifier ]);
                         ]
                       in
                       let headers =
                         Cohttp.Header.init_with "Content-Type"
                           "application/x-www-form-urlencoded"
                       in
                       let open Lwt.Infix in
                       Dream.log "Body %s" (Uri.encoded_of_query form_data);
                       fetch_follow_redirects ~headers
                         ~body:
                           (Cohttp_lwt.Body.of_string
                              (Uri.encoded_of_query form_data))
                         (Uri.of_string token_url)
                       >>= fun (_, body) ->
                       Cohttp_lwt.Body.to_string body >>= fun body_str ->
                       Dream.log "%s" body_str;
                       auth := body_str;
                       Dream.redirect req "/email")
                 else Dream.empty `Bad_Request);
         Dream.get "/email" (fun _req ->
             let headers =
               Dream.log "Bearer %s\n" !auth;
               Cohttp.Header.init_with "Authorization" ("Bearer " ^ !auth)
             in
             let open Lwt.Infix in
             Cohttp_lwt_unix.Client.post ~headers (Uri.of_string email_url)
             >>= fun (_res, body) ->
             Cohttp_lwt.Body.to_string body >>= fun body_str ->
             Dream.respond ~status:`OK body_str);
       ]
