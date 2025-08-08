open Angstrom
open Base

let ws =
  skip_while (function
    | ' ' | '\n' | '\t' | '\r' -> true
    | _ -> false)
;;

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
  | ' ' | '(' | ')' | ',' | '\n' | '\t' -> false
  | _ -> true
;;

let word = take_while1 is_word_char

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

type header =
  { level : int
  ; text : string list
  }
[@@deriving show]

let ilws =
  skip_while (function
    | ' ' | '\t' -> true
    | _ -> false)
;;

let text = sep_by ilws word

let heading =
  take_while1 (Char.equal '#')
  >>= fun heading_chars ->
  ilws *> text >>= fun t -> return { level = String.length heading_chars; text = t }
;;

type line =
  | Text of string list
  | Heading of header
  | ListItem of string list
  | EmptyLine
[@@deriving show]

let list_item = (char '-' <|> char '*') *> ilws *> text

let mdfile =
  sep_by
    (char '\n')
    (ilws *> peek_char
     >>= function
     | Some c ->
       (match c with
        | '#' -> heading <* ilws >>= fun h -> return (Heading h)
        | '-' | '*' -> list_item <* ilws >>= fun l -> return (ListItem l)
        | _ -> text <* ilws >>= fun t -> return (Text t))
     | None -> return EmptyLine)
;;

let%expect_test "test simple heading" =
  let text = "# hello" in
  match parse_string ~consume:All heading text with
  | Ok h ->
    Stdio.print_endline (show_header h);
    [%expect {| { Chess.level = 1; text = ["hello"] } |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "test simple 2 word heading" =
  let text = "# hello world" in
  match parse_string ~consume:All heading text with
  | Ok h ->
    Stdio.print_endline (show_header h);
    [%expect {| { Chess.level = 1; text = ["hello"; "world"] } |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "test simple level 2 heading" =
  let text = "## hello world" in
  match parse_string ~consume:All heading text with
  | Ok h ->
    Stdio.print_endline (show_header h);
    [%expect {| { Chess.level = 2; text = ["hello"; "world"] } |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "list item" =
  let text = "- some list item" in
  match parse_string ~consume:All list_item text with
  | Ok li ->
    List.iter li ~f:Stdio.print_endline;
    [%expect {|
      some
      list
      item
      |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "some whole file" =
  let text =
    {|## this is a heading

    * some list item
    * some list item

    text inbetween the list items

    - some list item
    - some list item 2

    #### heading in the wild
    
    text that goes over very two lines
    of text that goes over

    goes over two lines|}
  in
  match parse_string ~consume:All mdfile text with
  | Ok lines ->
    List.iter lines ~f:(fun l -> l |> show_line |> Stdio.print_endline);
    [%expect {|
      (Chess.Heading { Chess.level = 2; text = ["this"; "is"; "a"; "heading"] })
      (Chess.Text [])
      (Chess.ListItem ["some"; "list"; "item"])
      (Chess.ListItem ["some"; "list"; "item"])
      (Chess.Text [])
      (Chess.Text ["text"; "inbetween"; "the"; "list"; "items"])
      (Chess.Text [])
      (Chess.ListItem ["some"; "list"; "item"])
      (Chess.ListItem ["some"; "list"; "item"; "2"])
      (Chess.Text [])
      (Chess.Heading { Chess.level = 4; text = ["heading"; "in"; "the"; "wild"] })
      (Chess.Text [])
      (Chess.Text ["text"; "that"; "goes"; "over"; "very"; "two"; "lines"])
      (Chess.Text ["of"; "text"; "that"; "goes"; "over"])
      (Chess.Text [])
      (Chess.Text ["goes"; "over"; "two"; "lines"])
      |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

let%expect_test "some other example" =
  let text =
    {|# This is a simple paragraph
    
    This is some text angstrom is a parser combinator and it makes it
    super easy to parse any kinds of text with ease.

    And it is very cool that you can just make some small parsers
    and combine them to bigger cool parsers of e.g. markdown files.|}
  in
  match parse_string ~consume:All mdfile text with
  | Ok lines ->
    List.iter lines ~f:(fun l -> l |> show_line |> Stdio.print_endline);
    [%expect {|
      (Chess.Heading
         { Chess.level = 1; text = ["This"; "is"; "a"; "simple"; "paragraph"] })
      (Chess.Text [])
      (Chess.Text
         ["This"; "is"; "some"; "text"; "angstrom"; "is"; "a"; "parser";
           "combinator"; "and"; "it"; "makes"; "it"])
      (Chess.Text
         ["super"; "easy"; "to"; "parse"; "any"; "kinds"; "of"; "text"; "with";
           "ease."])
      (Chess.Text [])
      (Chess.Text
         ["And"; "it"; "is"; "very"; "cool"; "that"; "you"; "can"; "just"; "make";
           "some"; "small"; "parsers"])
      (Chess.Text
         ["and"; "combine"; "them"; "to"; "bigger"; "cool"; "parsers"; "of";
           "e.g."; "markdown"; "files."])
      |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;

type json =
  | JString of string
  | JNumber of int
  | JObject of (string * json) list
  | JList of json list
[@@deriving show]


let lex p = ws *> p <* ws
let quoted_string = char '"' *> take_while (fun c -> not (Char.equal c '"')) <* char '"'
let number = take_while1 Char.is_digit >>| Int.of_string

let json =
  fix (fun json ->
    let string_value = quoted_string >>| fun s -> JString s in
    let number_value = number >>| fun n -> JNumber n in
    let json_value = json >>| fun j -> JObject j in
    let value = ws *> choice [ string_value; number_value; json_value ] in
    let list = ws *> char '[' *> lex (sep_by (char ',' <* ws) value) <* char ']' >>| fun l -> JList l in 
    let key_value_pair =
      lex quoted_string
      >>= fun key -> lex (char ':') *> choice [ value; list ] >>| fun v -> key, v
    in
    let pairs = sep_by (lex (char ',')) key_value_pair in
    char '{' *> lex pairs <* char '}')
;;

let%expect_test "simple json parsing" =
  let text =
    {|{
    "name": "example",
    "count": 42,
    "nested": {
      "a": "b"
    },
    "list": [1, 2, 3, 4]
    }|}
  in
  match parse_string ~consume:All json text with
  | Ok r ->
    List.iter r ~f:(fun sl ->
      let s, l = sl in
      Stdio.printf "key: %s; value: %s\n" s (show_json l));
    [%expect {|
      key: name; value: (Chess.JString "example")
      key: count; value: (Chess.JNumber 42)
      key: nested; value: (Chess.JObject [("a", (Chess.JString "b"))])
      key: list; value: (Chess.JList
         [(Chess.JNumber 1); (Chess.JNumber 2); (Chess.JNumber 3);
           (Chess.JNumber 4)])
      |}]
  | Error err ->
    Stdio.print_endline err;
    [%expect.unreachable]
;;
