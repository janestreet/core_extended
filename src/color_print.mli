(* Ansi colored printing module *)

open Core.Std

(* Remove all special formatting and colors from string *)
val normal: string -> string

(* Turn strings various colors or add formatting.

   Orthogonal formatting settings should generally compose. For example, bold+red is
   possible. When settings conflict, if [override] is true, will attempt to modify the
   existing string to override the conflicting setting. Otherwise by default will preserve
   the existing formatting and only visibly apply the new setting to nonconflicting parts
   of the string. *)
val bold      : ?override:bool -> string -> string
val underline : ?override:bool -> string -> string
val inverse   : ?override:bool -> string -> string
val red       : ?override:bool -> string -> string
val yellow    : ?override:bool -> string -> string
val green     : ?override:bool -> string -> string
val blue      : ?override:bool -> string -> string
val magenta   : ?override:bool -> string -> string
val cyan      : ?override:bool -> string -> string

(* floats should be in [0,1], assumes 256 color terminal.
   If your terminal isn't displaying 256 colors, there's a good chance it still supports
   256 colors, and you might be able to get it by something like
   "export TERM=xterm-256color" *)
val gray: ?override:bool -> string -> brightness:float -> string
val rgb: ?override:bool -> string -> r:float -> g:float -> b:float -> string

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

val color: ?override:bool -> string -> color:color -> string

(* sprintf - Convenience functions. *)
val bold_sprintf     : ?override:bool -> ('a, unit, string, string) format4 -> 'a
val underline_sprintf: ?override:bool -> ('a, unit, string, string) format4 -> 'a
val inverse_sprintf  : ?override:bool -> ('a, unit, string, string) format4 -> 'a
val red_sprintf      : ?override:bool -> ('a, unit, string, string) format4 -> 'a
val yellow_sprintf   : ?override:bool -> ('a, unit, string, string) format4 -> 'a
val green_sprintf    : ?override:bool -> ('a, unit, string, string) format4 -> 'a
val blue_sprintf     : ?override:bool -> ('a, unit, string, string) format4 -> 'a
val magenta_sprintf  : ?override:bool -> ('a, unit, string, string) format4 -> 'a
val cyan_sprintf     : ?override:bool -> ('a, unit, string, string) format4 -> 'a

val gray_sprintf : ?override:bool -> brightness:float              -> ('a, unit, string, string) format4 -> 'a
val rgb_sprintf  : ?override:bool -> r:float -> g:float -> b:float -> ('a, unit, string, string) format4 -> 'a
val color_sprintf: ?override:bool -> color:color                   -> ('a, unit, string, string) format4 -> 'a


(* Formatted printf. *)
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

