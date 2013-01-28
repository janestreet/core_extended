type 'a t = 'a -> unit

type 'a inv = 'a t

module type S1 = sig
  type 'a t
  val invariant : 'a inv -> 'a t inv
end

module type S2 = sig
  type ('a, 'b) t
  val invariant : 'a inv -> 'b inv -> ('a, 'b) t inv
end

module type S3 = sig
  type ('a, 'b, 'c) t
  val invariant : 'a inv -> 'b inv -> 'c inv -> ('a, 'b, 'c) t inv
end
