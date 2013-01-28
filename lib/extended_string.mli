open Core.Std

(**
   Extensions to [Core.Core_String] .
*)

(**
   [collate s1 s2] sorts string in an order that's is usaully more suited
   for human consumption by treating ints specificaly:
   (e.g. it will output: [["rfc1.txt";"rfc822.txt";"rfc2086.txt"]]).

   It works by splitting the strings in numerical and non numerical chunks and
   comparing chunks two by two from left to right (and starting on a non
   numerical chunks):
   - Non_numerical chunks are compared using lexicographical ordering.
   - Numerical chunks are compared based on the values of the represented ints
   and the number of trailing zeros.

   It is a total order.
*)
val collate : string -> string -> int

(**
   [unescaped s] is the inverse operation of [escaped]: it takes a string where
   all the special characters are escaped following the lexical convention of
   OCaml and returns an unescaped copy.
   The [strict] switch is on by default and makes the function treat illegal
   backslashes as errors.
   When [strict] is [false] every illegal backslash except escaped numeral
   greater than [255] is copied literally. The aforementioned numerals still
   raise errors. This mimics the behaviour of the ocaml lexer.
*)
val unescaped : ?strict:bool -> string -> string

(**
   Same as [unescaped] but instead of raising [Failure _] returns an error
   message with the position in the string in case of failure.
*)
val unescaped_res : ?strict:bool -> string -> (string,(int*string)) Core.Result.t

(** [squeeze str] reduces all sequences of spaces, newlines, tables, and
 * carriage returns to single spaces.
 *)
val squeeze : string -> string

(** [is_substring ~substring t] returns [true] if substring is a substring
 * of t.
 *)
val is_substring : substring:string -> string -> bool

(** [pad_left ~char s len]
    Returns [s] padded to the length [len] by adding characters [char] to the
    left of the string. If s is already longer than [len] it is returned unchanged.
*)
val pad_left : ?char:char -> string -> int -> string
val pad_right : ?char:char -> string -> int -> string

(**deprecated in favour of word_wrap *)
val line_break: len:int -> string -> string list

(**
   [word_wrap ~soft_limit s]

   Wraps the string so that it fits the length [soft_limit]. It doesn't break
   words unless we go over [hard_limit].

   if [nl] is passed it is inserted instead of the normal newline character.
*)
val word_wrap:
  ?trailing_nl:bool
  -> ?soft_limit:int
  -> ?hard_limit:int
  -> ?nl:string
  -> string
  -> string

(** Consolidates a list of strings (almost [^]) losslessly.  E.g.:

    abc-def-1-ghijk
    abc-def-2-ghijk
    abc-def-5-ghijk
    abc-xyz-2
    abc-xyz-3

    becomes:

    abc-{def-[1-2,5]-ghijk,xyz-[2-3]}

    The algorithm is conceptually as follows:
    1) if all strings are sequences of digits, return [ contiguous subranges ],
    otherwise:
    2) split all strings into groups of consecutive letters or digits (but not both)
    3) break the list into sublists by the 1st and last token (1st token has precedence)
    4) for each sublist, find the longest prefix and suffix common for all entries
    5) replace each sublist with:
    "common_prefix-<result_of_the_recursive_call>-common_suffix"
    6) return { String.concat ~sep:"," <compressed_sublists> }

    In the implementation, we only tokenize the strings once, and then recursively work
    with lists of tokens.

   [^] Repeated entries and the original ordering of the list are not preserved.
   Otherwise, this transformation is lossless.

   Additionally, one may choose to represent sets of integers as the [lower..upper bound]
   (so [1-2,5] becomes [1..5]), or just by an asterisk ("*"), when brevity is preferred
   over accuracy.

   [^] Repeated entries and the original ordering of the list are not preserved.
   Otherwise, this transformation is lossless.
*)
val consolidate_strings :
  ?int_sets : [`Exact | `Bounds | `Asterisk]
  -> string list
  -> string


(** Consolidates a list of strings in such a way that the result does not exceed the given
    length.  Tries the following, in order:

    1) plain consolidate_strings
    2) consolidate_strings ~int_sets:`Bounds
    3) consolidate_strings ~int_sets:`Asterisk
    4) prefix of 3 ^ "..."
*)
val consolidate_strings' :
  max_len:int
  -> string list
  -> string

(** Gives the Levenshtein distance between 2 strings, which is the number of insertions,
    deletions, and substitutions necessary to turn either string into the other. With the
    [transpose] argument, it alsos considers transpositions (Damerau-Levenshtein
    distance). *)
val edit_distance : ?transpose : unit -> string -> string -> int
