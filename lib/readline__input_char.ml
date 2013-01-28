open Core.Std

type t = [
  | `Backspace
  | `Tab
  | `Newline
  | `Char of char
  | `Up
  | `Down
  | `Left
  | `Right
  | `Home
  | `End
  | `Delete
  | `Eof
  | `Unknown_escape of (string*int option*int option) ]

let char () = input_char stdin

(**
   An Ecma escape sequence is two characters separated by one or two optional
   numbers.

   This reads Ecma sequences from the stdin; it doesn't however read the escape
   character ["\027"]. It is based on specifications and reverse engineering...
*)
(*
  Does not handle all the bells and whistles of Ecma-48 because we only need
  to handle what the keyboard can reasonably output.
*)
let parse_esc ()=
  let b1 = Buffer.create 4
  and b2 = Buffer.create 4
  in
  let cmd = String.create 2 in
  cmd.[0] <- char ();
  let rec aux seen_semi =
    let c = char () in
    let b = if seen_semi then b2 else b1 in
    match c with
    | ';' when not seen_semi ->
        aux true
    | '0'..'9'  ->
       Buffer.add_char b c;
        aux seen_semi
    |'~' when Buffer.length b > 0 ->
       let c = Buffer.nth b 0 in
       let b_cnt = Buffer.sub b 1 (Buffer.length b -1) in
       Buffer.clear b;
       Buffer.add_string b b_cnt;
       c
    | _ -> c
  in
  let c = aux false in
  cmd.[1] <- c;
  let quant b = match Buffer.contents b with
    | "" -> None
    | s -> Some (int_of_string s)
  in
  cmd,quant b1,quant b2

let get () = match char () with
  | '\n' -> `Newline
  | '\t' -> `Tab
  | '\127' -> `Backspace
  | '\004' -> `Eof
  | '\027' -> (* Escape sequence *)
      (match (parse_esc ()) with
       | "[A",(None | Some 1),None -> `Up
       | "[B",(None | Some 1),None -> `Down
       | "[D",(None | Some 1),None -> `Left
       | "[C",(None | Some 1),None -> `Right
       | "[3",(None | Some 1),None -> `Delete
       | "OH",None,None -> `Home
       | "OF",None,None -> `End
       | v -> `Unknown_escape v)
  | c -> `Char c
