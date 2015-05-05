open Core_extended.Std
open Ascii_table

let data = [
  ["aaa"; "bbb"; "ccc   ooo       ooo\ntoto"; "ddd"; "eee"];
  ["aaa"; "bbb"; "ccc"; "ddd"; "eee\ntoto"];
  ["aaa"; "bbb"; "ccc"; "ddd"; "eee"];
  ["aaa"; "bbb"; "ccc"; "ddd"; "eee"];
  ["aaa"; "bbb"; "ccc"; "ddd"; "eee"];
]

let () =
  let col i = Column.create ("column " ^ Int.to_string i) (fun x -> List.nth_exn x i) in
  List.iter [
    "short_box", Display.short_box;
    "tall_box", Display.tall_box;
    "line", Display.line;
    "blank", Display.blank;
  ]
    ~f:(fun (name, display) ->
      Printf.printf "* %s:\n" name;
      output ~oc:stdout ~limit_width_to:50
        ~display ~bars:`Unicode
        [col 0; col 1; col 2; col 3; col 4] data)

