open Core.Std
open Profunctor_intf

module Record_builder(F : Strong) :
  Record_builder with type ('a, 'b) profunctor := ('a, 'b) F.t

module Of_applicative(F : Applicative) :
  Of_applicative with type 'a applicative := 'a F.t
