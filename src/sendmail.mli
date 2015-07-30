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
