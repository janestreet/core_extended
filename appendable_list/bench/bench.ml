open Base

module _ = struct
  let to_list l = l []
  let cons x l rest = x :: l rest
  let snoc l x rest = l (x :: rest)
  let empty rest = rest

  let%bench_fun ("cons+to_list" [@indexed n = [ 100; 10000 ]]) =
    fun () -> to_list (Fn.apply_n_times ~n (fun l -> cons 5 l) empty)
  ;;

  let%bench_fun ("snoc+to_list" [@indexed n = [ 100; 10000 ]]) =
    fun () -> to_list (Fn.apply_n_times ~n (fun l -> snoc l 5) empty)
  ;;
end

module _ = struct
  open Appendable_list

  let snoc l x = append l (singleton x)
  let cons x l = append (singleton x) l

  let%bench ("cons+to_list" [@indexed n = [ 100; 10000 ]]) =
    to_list (Fn.apply_n_times ~n (fun l -> cons 5 l) empty)
  ;;

  let%bench ("snoc+to_list" [@indexed n = [ 100; 10000 ]]) =
    to_list (Fn.apply_n_times ~n (fun l -> snoc l 5) empty)
  ;;

  let%bench_fun ("left-append+to_list" [@indexed n = [ 10; 1000 ]]) =
    let chunk = List.init 10 ~f:(fun i -> i) in
    fun () -> to_list (Fn.apply_n_times ~n (fun l -> append (of_list chunk) l) empty)
  ;;

  let%bench_fun ("right-append+to_list" [@indexed n = [ 10; 1000 ]]) =
    let chunk = List.init 10 ~f:(fun i -> i) in
    fun () -> to_list (Fn.apply_n_times ~n (fun l -> append l (of_list chunk)) empty)
  ;;
end

module _ = struct
  open List

  let empty = []
  let snoc l x = l @ [ x ]

  let%bench ("cons+to_list" [@indexed n = [ 100; 10000 ]]) =
    to_list (Fn.apply_n_times ~n (fun l -> cons 5 l) empty)
  ;;

  let%bench ("snoc+to_list" [@indexed n = [ 100 ]]) =
    to_list (Fn.apply_n_times ~n (fun l -> snoc l 5) empty)
  ;;
end
