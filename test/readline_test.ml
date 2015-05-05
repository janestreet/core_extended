open Core.Std
open Core_extended.Std

(* interactive readline test *)

let names = [
  "Till";
  "Bene";
  "Mark";
  "David";
  "Markus"
]

let rec loop f =
  match f () with
  | None -> ()
  | Some line ->
      Printf.printf "%S\n%!" line;
      loop f

let () =
  let tab_completion ~left ~right:_ =
    let last = List.last_exn (String.split left ~on:' ') in
    List.filter names ~f:(String.is_prefix ~prefix:last)
  in
  loop (Readline.input_line ~tab_completion)
