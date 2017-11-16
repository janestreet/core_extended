open Core
open Core_extended.Std
open Async

module Config = struct
  type t =
    { systems              : string Core_extended.Std.Sexp.Comprehension.t
    ; counterparty_numbers : int    Core_extended.Std.Sexp.Comprehension.t
    ; chars                : char   Core_extended.Std.Sexp.Comprehension.t
    ; dates                : Date.t Core_extended.Std.Sexp.Comprehension.t
    ;
    } [@@deriving sexp]

  let to_string t =
    sprintf "%s\n%s\n%s\n%s"
      (sprintf !"systems: %{Sexp}" ([%sexp_of: string list] t.systems))
      (sprintf !"counterparty_numbers: %{Sexp}" ([%sexp_of: int list] t.counterparty_numbers))
      (sprintf !"chars: %{Sexp}" ([%sexp_of: char list] t.chars))
      (sprintf !"dates: %{Sexp}" ([%sexp_of: Date.t list] t.dates))
end

let main config =
  Reader.load_sexp_exn config Config.t_of_sexp
  >>> fun config ->
  printf !"%{Config}\n" config;
  shutdown 0

let command =
  Command.basic_spec ~summary:"read a config file with an sexp comprehension"
    Command.Spec.(empty
                  +> flag "-config" (required file) ~doc:" config file")
    (fun config ->
      main config;
      never_returns (Scheduler.go ()))

let () = Command.run command
