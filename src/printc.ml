open Core

let (~:) l = String.concat l
let (~%) = sprintf

let print = print_string
let fprint = Out_channel.output_string
let eprint x = Out_channel.output_string Out_channel.stderr x

let printl s = print s; print "\n"; Out_channel.flush stdout
let fprintl f s = fprint f s; fprint f "\n"; Out_channel.flush stdout
let eprintl s = eprint s; eprint "\n"; Out_channel.flush stderr

let pad side ?(fill=' ') n s =
  let orig_len = String.length s in
  if orig_len >= n then s
  else
    let s' = Bytes.make n fill in
    let dst_pos = match side with
      | `left -> n - orig_len
      | `right -> 0
    in
    Bytes.From_string.blit ~src:s ~dst:s' ~src_pos:0 ~dst_pos ~len:orig_len;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:s'

let lpad ?fill n s = pad `left ?fill n s
let rpad ?fill n s = pad `right ?fill n s

let i2s = Int.to_string
let f2s = Float.to_string_12

(*
let f2s fmt f =
  let fmt = Scanf.sscanf (~:["\"";fmt;"\""]) "%{%f%}" (fun i -> i) in
  sprintf fmt f
*)
