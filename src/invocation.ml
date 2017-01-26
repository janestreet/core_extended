open Core

let full () = String.concat (Array.to_list Sys.argv) ~sep:" "

let commands_only () =
    (* We are looking for the name of the executable and the prefix
       that denotes the actual command run (e.g. db tools sql extract).
       Postgres also restricts the length of an application name to 64
       characters. As we don't actually know what is a flag or a
       filename and what is part of the command name we apply a heuristic.
    *)
  let cmd = Sys.argv.(0) |> Filename.basename in
  let args = List.tl_exn (Array.to_list Sys.argv) in
  let rec loop res args =
    match args with
    | [] -> res
    | a :: args ->
      if String.contains a '.' || String.contains a '/' || a = "" ||
        not ('a' <= a.[0] && a.[0] <= 'z') then
        res
      else
        loop (res ^ " " ^ a) args
  in
  loop cmd args
;;
