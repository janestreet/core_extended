(* Ansi colored printing module *)

open! Core

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
] [@@deriving sexp, bin_io]

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
(* Note that flushing stdout with "%!" does not work with these functions.  If you need to
   flush then you should use Out_channel.flush *)
val bold_printf     : ?override:bool -> ('a, unit, string, unit) format4 -> 'a
val underline_printf: ?override:bool -> ('a, unit, string, unit) format4 -> 'a
val inverse_printf  : ?override:bool -> ('a, unit, string, unit) format4 -> 'a
val red_printf      : ?override:bool -> ('a, unit, string, unit) format4 -> 'a
val yellow_printf   : ?override:bool -> ('a, unit, string, unit) format4 -> 'a
val green_printf    : ?override:bool -> ('a, unit, string, unit) format4 -> 'a
val blue_printf     : ?override:bool -> ('a, unit, string, unit) format4 -> 'a
val magenta_printf  : ?override:bool -> ('a, unit, string, unit) format4 -> 'a
val cyan_printf     : ?override:bool -> ('a, unit, string, unit) format4 -> 'a

val gray_printf : ?override:bool -> brightness:float              -> ('a, unit, string, unit) format4 -> 'a
val rgb_printf  : ?override:bool -> r:float -> g:float -> b:float -> ('a, unit, string, unit) format4 -> 'a
val color_printf: ?override:bool -> color:color                   -> ('a, unit, string, unit) format4 -> 'a

