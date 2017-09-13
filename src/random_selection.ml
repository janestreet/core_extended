(* reservoir sampling
   see http://www.nist.gov/dads/HTML/reservoirSampling.html
   see http://hnr.dnsalias.net/wordpress/?p=43
*)

open Core

type 'a t = {
  rand                : (int -> int)      ;
  desired_sample_size : int               ;
  selection           : 'a   option array ;
  num_seen            : int  ref          ;
}
[@@deriving fields]

let desired_sample_size t = t.desired_sample_size

let to_list t = List.filter_opt (Array.to_list t.selection)

let create ?random_state desired_sample_size =
  let rand =
    let state =
      match random_state with
      | None -> Random.State.default
      | Some state -> state
    in
    fun n -> Random.State.int state n
  in
  let selection = Array.create ~len:desired_sample_size None in
  { rand; desired_sample_size; selection; num_seen = ref 0 }

let maybe_add {rand; desired_sample_size; selection; num_seen} elem =
  incr num_seen;
  if !num_seen <= desired_sample_size then
    Array.set selection (!num_seen - 1) (Some elem)
  else begin
    if rand !num_seen < desired_sample_size then begin
      Array.set selection (rand desired_sample_size) (Some elem);
    end
  end

let select ?random_state ~next desired_sample_size =
  let t = create ?random_state desired_sample_size in
  let rec loop () =
    match next () with
    | None -> ()
    | Some elem -> maybe_add t elem; loop ()
  in
  loop ();
  to_list t

let clear t =
  Fields.iter
    ~rand:                ignore
    ~desired_sample_size: ignore
    ~selection:           (fun _ -> Array.iteri t.selection ~f:(fun i _ -> t.selection.(i) <- None))
    ~num_seen:            (fun _ -> t.num_seen := 0)

let num_seen t = !(t.num_seen)

let%expect_test _ =
  let size = 3 in
  let t = create ~random_state:(Random.State.make [| 17 |]) size in
  for i = 0 to 9 do
    maybe_add t i;
    printf !"num_seem=%d %{sexp#mach: int list}\n" (num_seen t) (to_list t);
  done;
  [%expect {|
    num_seem=1 (0)
    num_seem=2 (0 1)
    num_seem=3 (0 1 2)
    num_seem=4 (0 1 3)
    num_seem=5 (4 1 3)
    num_seem=6 (4 1 5)
    num_seem=7 (4 1 6)
    num_seem=8 (4 1 6)
    num_seem=9 (4 1 6)
    num_seem=10 (9 1 6) |}];
  clear t;
  for i = 0 to 9 do
    maybe_add t i;
    printf !"num_seem=%d %{sexp#mach: int list}\n" (num_seen t) (to_list t);
  done;
  [%expect {|
    num_seem=1 (0)
    num_seem=2 (0 1)
    num_seem=3 (0 1 2)
    num_seem=4 (3 1 2)
    num_seem=5 (3 1 4)
    num_seem=6 (3 1 4)
    num_seem=7 (6 1 4)
    num_seem=8 (6 7 4)
    num_seem=9 (6 7 4)
    num_seem=10 (6 7 4) |}]
;;
