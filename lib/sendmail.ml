(** Simple (and likely incomplete) interface for sending mail *)
(*
  Sendmail is specified in the LSB
  http://refspecs.linux-foundation.org/LSB_3.2.0/LSB-Core-generic/LSB-Core-generic/baselib-sendmail-1.html
  and should respect the rfc-5322
  http://tools.ietf.org/html/rfc5322.html

  Do not change antyhing in here if you haven't read the rfc.
*)
(*
  TODO: implement mime encoding...

  Email adr validation?
*)
open Core.Std

(* Sadly enough not all mta implement the rfc properly so we need to sniff them
   out. There's no reliable way to do so but most distributions rely on symlinks.
 *)
type mta =
  | Ssmtp
  | Sendmail
  | Unknown

(* Memo.unit isn't threadsafe. Multiple concurrent calls to sendmail can raise
   Lazy.Undefined *)
let mta_mutex = Mutex.create () ;;
let mta_memo =
  Memo.unit
    (fun () ->
      match
        Result.try_with (fun () ->
          Filename.basename (Shell.run_one "readlink" ["-f";"/usr/sbin/sendmail"]
          ))
      with
      | Ok "sendmail.sendmail" -> Sendmail
      | Ok "ssmtp" -> Ssmtp
      | _ -> Unknown
    )
;;
let mta () = Mutex.critical_section mta_mutex ~f:mta_memo ;;

let header k v buf nl =
  bprintf buf "%s%s"
    (Extended_string.word_wrap (k ^ ": " ^ v)
       ~nl:(nl^ " ")
       ~trailing_nl:false
       ~soft_limit:78
       ~hard_limit:998)
    nl

let send
    ?sender
    ?subject
    ?(cc=[])
    ?(bcc=[])
    ?(reply_to=[])
    ?content_type
    ~recipients
    body =
  let nl = match mta () with
    | Sendmail | Unknown -> "\r\n"
    | Ssmtp -> "\n" (* ssmtp really is a piece of junk... *)
  in
  let buf = Buffer.create (String.length body * 2) in
  let option key = Option.iter ~f:(fun v -> header key v buf nl)
  and list key = function
    | [] -> ()
    | l -> header key (String.concat ~sep:("," ^ nl ^ "   ") l) buf nl
  in
  option "From" sender;
  list "To" recipients;
  option "Subject" subject;
  option "Content-type" content_type;
  list "Cc" cc;
  list "Bcc" bcc;
  list "Reply-to" reply_to;
  bprintf buf "%s%s" nl body;
  let input = Buffer.contents buf in
  Shell.run ~input "/usr/sbin/sendmail" ["-t";"-oi"]
