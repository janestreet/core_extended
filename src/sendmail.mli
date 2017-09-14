val send :
  ?sender:string
  -> ?subject:string
  -> ?cc:string list
  -> ?bcc:string list
  -> ?reply_to:string list
  -> ?content_type:string
  -> ?message_id:string
  -> ?in_reply_to:string
  -> ?auto_generated:unit
  -> recipients:string list
  -> string
  -> unit
[@@deprecated "[since 2016-08] use [Async_smtp.Simplemail] instead"]

module Deprecated_use_async_smtp_std_simplemail : sig
  val send :
    ?sender:string
    -> ?subject:string
    -> ?cc:string list
    -> ?bcc:string list
    -> ?reply_to:string list
    -> ?content_type:string
    -> ?message_id:string
    -> ?in_reply_to:string
    -> ?auto_generated:unit
    -> recipients:string list
    -> string
    -> unit
end
