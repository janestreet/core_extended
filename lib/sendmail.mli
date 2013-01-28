
open Core.Std

val send :
  ?sender:string
  -> ?subject:string
  -> ?cc:string list
  -> ?bcc:string list
  -> ?reply_to:string list
  -> ?content_type:string
  -> recipients:string list
  -> string
  -> unit
