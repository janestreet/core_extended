(**
   Functional pretty printing.
*)

(**
   This is a pretty printing library originally written by Christian Lindig and
   released under the Copyleft licence for the
   {{:http://www.cminusminus.org}c--} project. The design of this library is
   discussed in the article
   "{{: http://citeseer.ist.psu.edu/lindig00strictly.html}Strictly pretty}".

   The pretty printer provided by the [Pp] module is intended for tree-like
   structures.  Documents are represented by an abstract type [t]. A document
   can be printed to a file or a string and during this process the pretty
   printer decides about the final outcome of the document. The only parameter
   it takes into account is the total line width and the number of characters a
   sub-document occupies.

   A document of type [t] can be very small entity like a single word.
   Functions combine small documents to larger ones which then can be pretty
   printed.

   @author Christian Lindig
   @author Till Varoquaux
   @see <http://legacy.cs.uu.nl/daan/download/pprint/pprint.html> PPrint.
 *)


type t

(** The empty document is pretty printed to the empty string. Typically [empty]
is used in one branch of an [if] statement. *)
val empty : t

(** The infix operator [x $ y] concatenates two documents [x] and [y] into a
larger one.  It does not introduce any space or other separation between the two
original documents. *)
val ($) : t -> t -> t

(** The [text] function turns a string into a document. The pretty printed
representation of the resulting document is exactly the string [text] was
applied to.*)
val text : string -> t

(** The important points in a document are so-called [break]s.  A [break] can be
either represented by a single space or a newline followed by a number of spaces.
The pretty printer makes this decision based on the available space.  So think
of a [break] as a space that might come out as a newline.  To give the pretty
printer enough flexibility documents must be joined with [break]s:
[x $ break $ y]. *)
val break : t

(** [break_null] behaves like [break] except it does not insert anything when no
newline is inserted.*)
val break_null : t

(** The space character used by [break] my be not always appropriate. The
function [break_with s] behaves like [break] except that it uses a user supplied
string [s] instead of the space.*)
val break_with : string -> t

(**{3 Grouping and nesting}
The pretty printer considers the representation of [break]s not one by one but
looks at all [break]s of a sub-document. Documents are structured into
sub-documents by group-operators. Different group operators exist to control the
behavior of the pretty printer. *)


(** When the pretty printer decides to represent a [break] as a newline it also
prints some spaces after it to indent the following line. The number of spaces
is controlled by the [nest] operator that takes a document as argument:
[nest n d]. All breaks turned to newlines inside document [d] are followed by [n]
spaces.  The [nest] operator nests properly such that it takes the spaces
introduced by [nest]s on the outer level also into account.
*)
val nest : int -> t -> t

(** A group operator takes a document and let it become a group.  The [hgrp]
operator creates a {i horizontal} group.  Breaks inside a [hgrp] are never
turned into newlines but always come out as spaces. This group has a very
limited usefulness because it easily overruns any given line length. *)
val hgrp : t -> t

(** The [vgrp] operator creates a {i vertical} group. All [break]s inside a
[vgrp] are represented as newlines followed by spaces. Although all [break]s
come out as newlines the indentation of lines inside the group may differ:
nesting is independent of grouping and thus different nesting levels can be
active in the same group.  Because of the fixed pretty printing strategy [vgrp]s
are used mostly at the top level of documents only.*)
val vgrp : t -> t

(** The {i automatic} group [agrp] is the most versatile. Breaks inside this
group are either all turned into newlines (followed by spaces), or into spaces.
Subgroups are, of course, not affected but considered individually.*)
val agrp : t -> t

(** The break policy inside an [agrp] is fixed for all breaks of the group.
Inside a {i flexible} group [fgrp] each [break] is considered individually: when
the document up to the next [break] fits into the current line the [break] comes
out as space. Otherwise it comes out as newline followed by spaces.*)
val fgrp : t -> t

(**{3 Pretty Printing} *)

val to_string : ?width:int -> t -> string
val to_file : ?width:int -> out_channel -> t -> unit

(**{3 Auxiliaries}*)

(** A list of objects which are seperated by some seperator is very common. The
[list sep f] function takes care to insert the separator only between objects
but not at the end of the list. It creates a [sep] separated list.  Individual
items are printed using [f]. For the common case where commas are used for
separating we also provide an extra definition*)
val list      : sep:t -> f:('a -> t) -> 'a list -> t
val commalist : f:('a -> t) -> 'a list -> t

(** Instead of writing [x $ break $ y] to insert a [break] it is convenient to
define an operator for this: [x ^/ y] joins [x] and [y] with a [break]. *)
val ($/)      : t -> t -> t

(** Joins two documents with a [break_null].*)
val ($//)     : t -> t -> t

(** A [block] contains objects [xs] formatted by [f] and enclosed by curly
braces. Its body will be indented in case it does not fit on a single line.
The default indentation is 4
*)
val block     : ?indent:int -> f:('a -> t) -> 'a list -> t

module Infix : sig
  val ($) : t -> t -> t
  val ($/) : t -> t -> t
  val ($//) : t -> t -> t
end

(** {[ hlist [x1,..,xn] = hgrp [x1; break; x2; ...; break; xn) ]} *)
val hlist : t list -> t

(** {[ vlist [x1,..,xn] = vgrp [x1; break; x2; ...; break; xn) ]} *)
val vlist : t list -> t

val alist : t list -> t
