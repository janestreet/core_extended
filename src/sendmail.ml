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
open Core

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
           match Shell.run_one_line "readlink" ["-f";"/usr/sbin/sendmail"] with
           | Error _ -> assert false
           | Ok path -> Filename.basename path)
       with
       | Ok "sendmail.sendmail" -> Sendmail
       | Ok "ssmtp" -> Ssmtp
       | _ -> Unknown
    )
;;
let mta () = Mutex.critical_section mta_mutex ~f:mta_memo ;;

let header k v buf nl =
  Printf.bprintf buf "%s%s"
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
      ?message_id
      ?in_reply_to
      ?auto_generated
      ~recipients
      body =
  let nl = match mta () with
    | Sendmail | Unknown -> "\r\n"
    | Ssmtp -> "\n" (* ssmtp really is a piece of junk... *)
  in
  let buf = Buffer.create (String.length body * 2) in
  let option key = Option.iter ~f:(fun v -> header key v buf nl) in
  let list key = function
    | [] -> ()
    | l -> header key (String.concat ~sep:("," ^ nl ^ "   ") l) buf nl
  in
  (* Both the [Auto-Submitted] and [Precedence] headers are used to indicate an
     auto-generated email. To improve the odds of working with an unknown mail server,
     send both headers. *)
  let auto_generated_headers () =
    header "Auto-Submitted" "auto-generated" buf nl;
    header "Precedence" "bulk" buf nl
  in
  option "From" sender;
  list "To" recipients;
  option "Subject" subject;
  option "Content-type" content_type;
  list "Cc" cc;
  list "Bcc" bcc;
  list "Reply-to" reply_to;
  option "Message-ID" message_id;
  option "In-Reply-To" in_reply_to;
  Option.iter auto_generated ~f:auto_generated_headers;
  Printf.bprintf buf "%s%s" nl body;
  let input = Buffer.contents buf in
  Shell.run ~input "/usr/sbin/sendmail" ["-t";"-oi"]

module Deprecated_use_async_smtp_std_simplemail = struct
  let send = send
end
