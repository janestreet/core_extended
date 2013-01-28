(* Ansi colored printing module *)

open Core.Std

(* Remove all special formatting and colors from string *)
val normal: string -> string

(* Turn strings various colors or add formatting.
   Each call will override any existing colors or special formatting in the string *)
val bold      : string -> string
val underline : string -> string
val inverse   : string -> string
val red       : string -> string
val yellow    : string -> string
val green     : string -> string
val blue      : string -> string
val magenta   : string -> string
val cyan      : string -> string

(* floats should be in [0,1], assumes 256 color terminal.
   If your terminal isn't displaying 256 colors, there's a good chance it still supports
   256 colors, and you might be able to get it by something like
   "export TERM=xterm-256color" *)
val gray: string -> brightness:float -> string
val rgb: string -> r:float -> g:float -> b:float -> string

type color = [
(* Roughly sorted in order of hue *)
| `Black | `Gray | `Light_gray | `White
| `Dark_red | `Red | `Pink | `Light_pink
| `Orange | `Amber
| `Dark_yellow | `Gold | `Yellow | `Khaki | `Wheat
| `Chartreuse | `Green_yellow
| `Dark_green | `Green | `Light_green | `Bright_green
| `Spring_green | `Medium_spring_green
| `Dark_cyan | `Sea_green | `Cyan | `Turquoise | `Pale_turquoise
| `Dodger_blue | `Deep_sky_blue
| `Dark_blue | `Blue | `Light_slate_blue | `Light_steel_blue
| `Blue_violet | `Violet
| `Dark_magenta | `Purple | `Magenta | `Orchid | `Plum
| `Rose | `Deep_pink
] with sexp, bin_io

val color: string -> color:color -> string

(* dwu: Note that unlike the above functions, convenience functions might not override
   the entire string to be the right color in the case that the printed string has
   existing colors or formatting.
   I could not find a way to both make it correct and get the resulting type to unify
   with that of printf. *)
(* Convenience functions. *)
val boldprintf     : ('a, out_channel, unit) format -> 'a
val underlineprintf: ('a, out_channel, unit) format -> 'a
val inverseprintf  : ('a, out_channel, unit) format -> 'a
val redprintf      : ('a, out_channel, unit) format -> 'a
val yellowprintf   : ('a, out_channel, unit) format -> 'a
val greenprintf    : ('a, out_channel, unit) format -> 'a
val blueprintf     : ('a, out_channel, unit) format -> 'a
val magentaprintf  : ('a, out_channel, unit) format -> 'a
val cyanprintf     : ('a, out_channel, unit) format -> 'a

val grayprintf : brightness:float              -> ('a, out_channel, unit) format -> 'a
val rgbprintf  : r:float -> g:float -> b:float -> ('a, out_channel, unit) format -> 'a
val colorprintf: color:color                   -> ('a, out_channel, unit) format -> 'a

(* module Style : sig
 *   type t
 *   val ansi : t
 *   val no_formatting : t
 *   (\* no ansi formatting, but prints "not ok:" for yellow or red and "ok:" for green *\)
 *   val ok_or_not : t
 * end
 *
 * module type T = sig
 *   val println        : string -> unit
 *   val redprintln     : string -> unit
 *   val yellowprintln  : string -> unit
 *   val greenprintln   : string -> unit
 *   val blueprintln    : string -> unit
 *   val inverseprintln : string -> unit
 *   val printfln        : ('a, out_channel, unit) format -> 'a
 *   val redprintfln     : ('a, out_channel, unit) format -> 'a
 *   val yellowprintfln  : ('a, out_channel, unit) format -> 'a
 *   val greenprintfln   : ('a, out_channel, unit) format -> 'a
 *   val blueprintfln    : ('a, out_channel, unit) format -> 'a
 *   val inverseprintfln : ('a, out_channel, unit) format -> 'a
 * end
 *
 * type t = (module T)
 *
 * val create :
 *   style:Style.t
 *   -> oc:Out_channel.t
 *   -> t
 *
 * (\* ansi on standard out *\)
 * module S : T *)

