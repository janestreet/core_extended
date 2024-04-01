(** Immediate versions of standard datatypes. *)

open Immediate_intf
include Immediate_kernel

module type Intern_table = Intern_table
module type String_no_option = String_no_option
module type String_option = String_option
module type String = Immediate_string_intf.S

(** There is no facility to un-intern an Interned_string.  They are remembered forever. *)
module Interned_string = Immediate_interned_string

module Short_string = Immediate_short_string
module String = Immediate_string
module Stringable = Immediate_stringable
module Identifiable = Immediate_identifiable
module Immediate_stable = Immediate_stable
