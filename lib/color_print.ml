open Core.Std

module Style = struct
  type 'a fmt = ('a,out_channel,unit) format

  module type T = sig
    val red     : 'a fmt -> 'a fmt
    val yellow  : 'a fmt -> 'a fmt
    val green   : 'a fmt -> 'a fmt
    val blue    : 'a fmt -> 'a fmt
    val inverse : 'a fmt -> 'a fmt
  end

  type t = (module T)

  let ansi =
    let module M = struct
      let style code fmt = "\027[" ^^ code ^^ "m" ^^ fmt ^^ "\027[0m"

      let red     fmt = style "31" fmt
      let yellow  fmt = style "33" fmt
      let green   fmt = style "32" fmt
      let blue    fmt = style "34" fmt
      let inverse fmt = style "7"  fmt
    end
    in
    (module M : T)

  let no_formatting =
    let module M = struct
      let red      = Fn.id
      let yellow   = Fn.id
      let green    = Fn.id
      let blue     = Fn.id
      let inverse  = Fn.id
    end
    in
    (module M : T)

  let ok_or_not =
    let module M = struct
      let ok     fmt = "ok:     " ^^ fmt
      let not_ok fmt = "not ok: " ^^ fmt

      let red     = not_ok
      let yellow  = not_ok
      let green   = ok
      let blue    = Fn.id
      let inverse = Fn.id
    end
    in
    (module M : T)
end

module type T = sig
  val println        : string -> unit
  val redprintln     : string -> unit
  val yellowprintln  : string -> unit
  val greenprintln   : string -> unit
  val blueprintln    : string -> unit
  val inverseprintln : string -> unit
  val printfln        : 'a Style.fmt -> 'a
  val redprintfln     : 'a Style.fmt -> 'a
  val yellowprintfln  : 'a Style.fmt -> 'a
  val greenprintfln   : 'a Style.fmt -> 'a
  val blueprintfln    : 'a Style.fmt -> 'a
  val inverseprintfln : 'a Style.fmt -> 'a
end

type t = (module T)

let create ~style ~oc =
  ignore style;
  let module M = struct
    include (val style : Style.T)

    let nl_flsh fmt = fmt ^^ "\n%!"
    let nl_flsh_fmt = nl_flsh "%s"

    let println        = fprintf oc nl_flsh_fmt
    let redprintln     = fprintf oc (red     nl_flsh_fmt)
    let yellowprintln  = fprintf oc (yellow  nl_flsh_fmt)
    let greenprintln   = fprintf oc (green   nl_flsh_fmt)
    let blueprintln    = fprintf oc (blue    nl_flsh_fmt)
    let inverseprintln = fprintf oc (inverse nl_flsh_fmt)

    let printfln        fmt = fprintf oc (nl_flsh fmt)
    let redprintfln     fmt = fprintf oc (nl_flsh (red fmt))
    let yellowprintfln  fmt = fprintf oc (nl_flsh (yellow fmt))
    let greenprintfln   fmt = fprintf oc (nl_flsh (green fmt))
    let blueprintfln    fmt = fprintf oc (nl_flsh (blue fmt))
    let inverseprintfln fmt = fprintf oc (nl_flsh (inverse fmt))
  end
  in
  (module M : T)

module S = (val create ~style:Style.ansi ~oc:stdout : T)
