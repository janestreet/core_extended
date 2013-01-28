open Core.Std

(** This is a port of Bram Cohen's patience diff algorithm, as found in the Bazaar 1.14.1
    source code, available at http://bazaar-vcs.org.

    This copyright notice was included:

    # Copyright (C) 2005 Bram Cohen, Copyright (C) 2005, 2006 Canonical Ltd
    #
    # This program is free software; you can redistribute it and/or modify
    # it under the terms of the GNU General Public License as published by
    # the Free Software Foundation; either version 2 of the License, or
    # (at your option) any later version.
    #
    # This program is distributed in the hope that it will be useful,
    # but WITHOUT ANY WARRANTY; without even the implied warranty of
    # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    # GNU General Public License for more details.
    #
    # You should have received a copy of the GNU General Public License
    # along with this program; if not, write to the Free Software
    # Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)


(** Bram Cohen's comment from the original Python code (with syntax changed to OCaml):

    [get_matching_blocks a b] returns a list of triples describing matching
    subsequences.

    Each triple is of the form (i, j, n), and means that
    a <|> (i,i+n) = b <|> (j,j+n).  The triples are monotonically increasing in
    i and in j.

    The last triple is a dummy, (Array.length a, Array.length b, 0), and is the only
    triple with n=0.

    Example:
    get_matching_blocks [|"a";"b";"x";"c";"d"|] [|"a";"b";"c";"d"|]
    returns
    [(0, 0, 2), (3, 2, 2), (5, 4, 0)]
*)
module Matching_block : sig
  type t = {
    mine_start  : int;
    other_start : int;
    length      : int;
  }
end

val get_matching_blocks :
  transform: ('a -> 'b)
  -> compare: ('b -> 'b -> int)
  -> mine:'a array
  -> other:'a array
  -> Matching_block.t list

val ratio : 'a array -> 'a array -> float

(** For handling diffs abstractly.  A range is a subarray of the two original
    arrays with a constructor defining its relationship to the two original
    arrays.  A [Same] range contains a series of elements which can be found in
    both arrays.  A [New] range contains elements found only in the new array,
    while an [Old] range contains elements found only in the old array.

    A [Replace] contains two arrays: elements in the first array are elements
    found only in the original, old array which have been replaced by elements
    in the second array, which are elements found only in the new array.

*)
module Range : sig
  type 'a t =
      | Same of ('a * 'a) array
      | Old of 'a array
      | New of 'a array
      | Replace of 'a array * 'a array
      | Unified of 'a array
end

(** In diff terms, a hunk is a unit of consecutive ranges with some [Same]
    context before and after [New], [Old], and [Replace] ranges.  Each
    hunk contains information about the original arrays, specifically the
    starting indexes and the number of elements in both arrays to which
    the hunk refers.

    Furthermore, a diff is essentially a list of hunks.  The simplest case
    is a diff with infinite context, consisting of exactly one hunk. *)
module Hunk : sig
  type 'a t = {
    mine_start: int;
    mine_size: int;
    other_start: int;
    other_size: int;
    ranges: 'a Range.t list;
  }

  (** [all_same hunk] returns true if [hunk] contains only Same ranges. *)
  val all_same: 'a t -> bool
end


(** [get_hunks a b ~context ~compare] will compare the arrays [a] and [b] using
    [compare] and produce a list of hunks. (The hunks will contain Same ranges
    of at most [context] elements.)  [context] defaults to infinity (producing a
    singleton hunk list), [compare] defaults to polymorphic compare. *)
val get_hunks :
  transform: ('a -> 'b)
  -> compare: ('b -> 'b -> int)
  -> context: int
  -> mine: 'a array
  -> other: 'a array
  -> 'a Hunk.t list

val print_ranges : string Hunk.t -> unit

(** [get_status hunks] returns `Same if each hunk in [hunks] has only Same ranges. *)
val all_same : 'a Hunk.t list -> bool

(** [unified hunks] converts all Replace ranges in hunks to an Old range
    followed by a New range. *)
val unified : 'a Hunk.t list -> 'a Hunk.t list

(** [old_only hunks] drops all New ranges from hunks and converts all Replace
    ranges to Old ranges. *)
val old_only : 'a Hunk.t list -> 'a Hunk.t list

(** [new_only hunks] drops all Old ranges from hunks and converts all Replace
    ranges to New ranges. *)
val new_only : 'a Hunk.t list -> 'a Hunk.t list


type 'a segment =
    | Same of 'a array
    | Different of 'a array array

type 'a merged_array = 'a segment list

val merge : 'a array array -> 'a merged_array
